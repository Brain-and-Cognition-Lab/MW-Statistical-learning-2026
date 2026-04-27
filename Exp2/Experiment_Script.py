# -*- coding: utf-8 -*- 
"""
@author: phblonde
"""


#########   
# SETUP # 
#########
# ===== Import =====
# %%  
import pylink  # EyeLink library for tracking eye movements
import subprocess  # Library to run external commands, like converting EDF to ASC
from psychopy import core, visual, gui, data, event, monitors  # Psychopy for experimental design and stimuli presentation
from string import ascii_letters, digits  # For allowed characters in filenames
from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy  # For integrating EyeLink with PsychoPy
from numpy import tan, deg2rad, random  # For mathematical operations related to visual angle and generating random numbers
import numpy
from scipy import stats as stats  # For statistical functions, e.g., random uniform distribution
import random as rd # For generating random numbers
from itertools import permutations
import pandas as pd  # For managing data in DataFrame format
import datetime  # For working with dates and times
import tkinter as tk  # For GUI elements like input dialogs
import os as os  # For interacting with the file system
import math
from scipy.ndimage import gaussian_filter
# %% 

# ===== Definitions =====
# %%  
def deg_to_pix(deg, monitor):
    """Convert degrees of visual angle to pixels."""  
    return tan(deg2rad(deg)) * monitor.getDistance() * (monitor.getSizePix()[0] / monitor.getWidth())  # Convert degrees to pixels based on screen size and distance

def deg_to_cm(deg, distance_cm):
    return math.tan(math.radians(deg)) * distance_cm

def make_color_noise_mask(width=400, height=400,
                          correlation=2.0,
                          dark_threshold=30,
                          pixel_size=4):
    """
    Generates correlated color noise with blocky patches.
    Ensures no block is too dark, applied at *block level*.
    """
    # Low-res dimensions
    h_low = height // pixel_size
    w_low = width  // pixel_size

    # Generate low-res random noise [0..1] for 3 channels
    noise = random.rand(h_low, w_low, 3)

    # Apply Gaussian smoothing in low-res space
    smooth = gaussian_filter(noise, sigma=(correlation, correlation, 0), mode="wrap")

    # Normalize [0..255]
    smooth = (smooth - smooth.min()) / (smooth.max() - smooth.min() + 1e-9)
    arr_low = (smooth * 255).astype(numpy.uint8)

    # --- Ensure no dark blocks (replacement at low-res resolution) ---
    brightness = arr_low.mean(axis=2)
    dark_mask = brightness < dark_threshold
    if numpy.any(dark_mask):
        arr_low[dark_mask] = random.randint(0, 256, (numpy.sum(dark_mask), 3),
                                            dtype=numpy.uint8)

    # Upscale to full resolution (nearest-neighbour repeat)
    arr = arr_low.repeat(pixel_size, axis=0).repeat(pixel_size, axis=1)

    # Crop to exact requested size
    arr = arr[:height, :width, :]

    # Convert to PsychoPy range [-1, 1]
    arr = (arr.astype(numpy.float32) / 127.5) - 1.0
    return arr

# %% 

# ===== Base setup =====
# %%  
# Path setup  
#path = "C:/Users/Lab/Documents/MW_Stats_5"
path = "C:/Users/phili/Dropbox/Polish Postdoc/MW_Stats_6"
os.chdir(path)  # Change working directory for saving data
edf_path = path + "/Eye Data"  # Path for saving EyeLink data
csv_path = path + "/Behavioural Data"  # Path for saving behavioral data
Data_list = os.listdir(csv_path)  # List of existing EDF files for checking naming conventions

# Experiment setup dialog
expInfo = {'Initials': '', 'Age': 0, 'Gender': ["Female", "Male", "Other"], 
           'Dominant eye': ["Left", "Right"], 'Test Mode': ''}  # Dictionary for participant info

expInfo['Date'] = data.getDateStr()  # Add current date to the experiment info

# Gather base info and Set up EyeLink data file
while True:
    dlg = gui.DlgFromDict(expInfo, title='Cognitive psychology experiment', fixed=['Date'])  # Create GUI dialog for participant info input
    allowed_char = ascii_letters + digits + '_'  # Allowed characters for filenames
    if expInfo['Gender'] == "Female":
        expInfo['Gender'] = 'F'
    elif expInfo['Gender'] == "Male":
        expInfo['Gender'] = 'M'
    elif expInfo['Gender'] == "Other":
        expInfo['Gender'] = 'O'
    if len(Data_list) < 9:
        edf_filename = "0" + str(len(Data_list)+1) + expInfo['Initials'] + str(expInfo['Age']) + expInfo['Gender']  # File naming convention with leading zero if less than 9 files
    else:
        edf_filename = str(len(Data_list)+1) + expInfo['Initials'] + str(expInfo['Age']) + expInfo['Gender']  # File naming convention without leading zero

    # Check for invalid characters in the filename
    if not all(c in allowed_char for c in edf_filename):
        
        print('ERROR: Invalid character included in the filename')
    elif len(edf_filename) > 8:
        print(edf_filename)
        print('ERROR: EDF filename should not exceed 8 characters')
    elif len(edf_filename) == 0:
        print('ERROR: EDF filename is empty')
    else:
        print('The filename you specified is:', edf_filename)  # Print the final filename
        break  # Exit loop when filename is valid
    

# Trial number setup
#experiment_trial = (192*3)  # Number of trials (576 in this case)
training_trial = 10
# %% 

# ===== Dataframe setup =====
# %%  
# Data dictionary to store all experiment data for each trial  
df = {"Initials": {}, "Codename": {},"Age": {}, "Gender": {}, "Sequence": {}, "Dominant_Eye": {}, "Block": {}, "Test_Mode": {}, "Trial": {}, "Trial_Block": {}, 
      "Target_Direction": {}, "Target_Coordinates": {}, "Target_Quadrant": {}, "Target_Quadrant_Name": {},
      "Target_X": {}, "Target_Y": {}, "Quadrant_Type": {},"Bigram": {},"Bigram_Type": {}, "Response": {}, "Response_Correctness": {},
      "Probe_Number": {}, "Probe_Response": {}, "RT": {}, "Experiment_Start": {}, "Experiment_End": {}, "Total_Duration": {}, "Display_set": {}}
# %% 

# ===== GUI setup =====
# %%  
# Preparing the Experiment Window
root = tk.Tk()  # Initialize Tkinter for window setup
screen_width = root.winfo_screenwidth()  # Get the screen width in pixels
screen_height = root.winfo_screenheight()  # Get the screen height in pixels


ExpMonitor = monitors.Monitor('demoMon', width=53, distance=92)  # Define monitor properties (width and distance from the eye)
ExpMonitor.setSizePix((screen_width, screen_height))  # Set monitor size in pixels

win = visual.Window([screen_width, screen_height], allowGUI=True, monitor=ExpMonitor, units='deg', fullscr=False)  # Create PsychoPy window for display
win.setColor([98.33, 98.74, 104.14], 'rgb255')  # Set background color of the window to dark grey
win.flip()  # Refresh window to apply color

# Get window size
w, h = win.size
hw, hh = w // 2, h // 2

# Define quadrant positions and colors
quadrants = [
    {"pos": (-hw/2,  hh/2), "color": [90, 95, 130]},    # Grayish blue
    {"pos": ( hw/2,  hh/2), "color": [130, 95, 95]},    # Muted red
    {"pos": (-hw/2, -hh/2), "color": [130, 130, 95]},   # Soft yellow
    {"pos": ( hw/2, -hh/2), "color": [95, 130, 95]}     # Soft green
]


# Slider for mind-wandering ratings
slider = visual.Slider(win, ticks=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), pos=(0, 0), size=(15, 1), units="deg", granularity=1, 
                       labelColor="White", markerColor="Red", lineColor="White", colorSpace="rgb", autoLog=True, 
                       fillColor="Red", borderColor=True)

# Custom labels for slider
label_left = visual.TextStim(win, text="Zupełnie\nskupiony", pos=(-7.5, -2), units="deg", color="White", wrapWidth=6)
label_right = visual.TextStim(win, text="Zupełnie\nrozproszony", pos=(7.5, -2), units="deg", color="White", wrapWidth=6)

# Calculate slider boundaries in pixels
slider_x, slider_y = slider.pos  # Slider center position
left_bound = slider_x - 15.2 / 2  # Left boundary of the slider
right_bound = slider_x + 15.2 / 2  # Right boundary of the slider
top_bound = slider_y + 1.1 / 2  # Top boundary of the slider
bottom_bound = slider_y - 1.1 / 2  # Bottom boundary of the slider

print("Monitor Size (pixels):", screen_width, screen_height)
print("Monitor Size (cm):", ExpMonitor.getWidth())
print("Viewing Distance (cm):", ExpMonitor.getDistance())
print("Degrees 10° = {:.2f} cm".format(deg_to_cm(10, ExpMonitor.getDistance())))

# Creating the fixation cross
fixation = visual.ShapeStim(win, pos = [0,0], units='deg', vertices=((0,-1), (0,1), (0,0), (-1,0), (1,0)), 
                            lineWidth=5, closeShape=False, lineColor="white")

# %% 
# ===== Eye tracker setup =====
# %%  
# Set to True for dummy mode (simulated tracking without actual device)
DUMMY_MODE = True

# Initialize EyeLink connection
if DUMMY_MODE:
    el_tracker = pylink.EyeLink(None)  # Dummy EyeLink connection
else: 
    try:
        el_tracker = pylink.EyeLink('100.1.1.1')  # Establish connection to EyeLink device (use actual IP)
    except RuntimeError as err:
        print(err, '\nDouble-check the IP address of the stimulus presentation PC!')  # Error if the connection fails

# EyeLink tracker setup
el_tracker.setOfflineMode()  # Put the tracker in idle mode before changing parameters
el_tracker.openDataFile(edf_filename)  # Open an EDF data file to store gaze data
el_tracker.sendCommand("add_file_preamble_text 'Probability cueing task'")  # Add custom text to file preamble
el_tracker.sendCommand("file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK")  # Set event filters
el_tracker.sendCommand("file_sample_data  = LEFT,RIGHT,GAZE,GAZERES,PUPIL,HREF,AREA,STATUS,INPUT")  # Specify data to be stored
el_tracker.sendCommand("sample_rate 500")  # Set sample rate to 1000 Hz
el_tracker.sendCommand("calibration_type = HV9")  # Set the calibration type (HV9 for 9-point calibration)

# Initialize graphics for EyeLink
graphics = EyeLinkCoreGraphicsPsychoPy(el_tracker, win)  # Use PsychoPy window for EyeLink calibration
pylink.openGraphicsEx(graphics)  # Open EyeLink graphics interface
# %% 

# ===== Quadrant setup =====
# %%  
# Setup quadrant positions for target display
Quadrant_Type = []  # List to store quadrant types (Expected or Unexpected)

all_perms = list(permutations([1, 2, 3, 4]))

# Remove ascending and descending sequences (all rotations)
patterns = [
    (1,2,3,4), (2,3,4,1), (3,4,1,2), (4,1,2,3),  # ascending
    (4,3,2,1), (3,2,1,4), (2,1,4,3), (1,4,3,2),   # descending
    (1,2,4,3), (2,4,3,1), (4,3,1,2), (3,1,2,4),   # clockwise
    (4,2,1,3), (2,1,3,4), (1,3,4,2), (3,4,2,1)   # counterclockwise
]

sequence = list(rd.choice([p for p in all_perms if p not in patterns]))

total_sequence = []
for i in range(1000):
    if i == 0:
        total_sequence.append(rd.randint(1,4))
        Quadrant_Type.append("")
    else:
        current_num = total_sequence[-1]
        current_index = sequence.index(current_num)
        next_in_sequence = sequence[(current_index + 1) % 4]
        
        if rd.random() <= 0.6:
            total_sequence.append(next_in_sequence)
            Quadrant_Type.append("Expected")
        else:
            options = [x for x in [1,2,3,4] if x not in (current_num, next_in_sequence)]
            total_sequence.append(rd.choice(options))
            Quadrant_Type.append("Unexpected")


List_pos = list()  # List to store target positions
# %%  

# ===== Probe setup =====
# %%  
# Create probe locations for mind-wandering probes (20 probes placed at random intervals)

#NOMBRE PROBES

Probe_Location = []
for probe in range(20):
    if Probe_Location == []:
        Probe_Location.append(random.randint(round(len(total_sequence) / 20)-(round(len(total_sequence) / 20) * 10 / 100), round(len(total_sequence) / 20)+(round(len(total_sequence) / 20) * 10 / 100)))
    elif probe == 9:
        Probe_Location.append(int(len(total_sequence)/2)-1) 
    elif probe == 19:
        Probe_Location.append(int(len(total_sequence))-1)
    else:
        Probe_Location.append(Probe_Location[probe-1] + random.randint(round(len(total_sequence) / 20)-(round(len(total_sequence) / 20) * 10 / 100), round(len(total_sequence) / 20)+(round(len(total_sequence) / 20) * 10 / 100)))
        if (len(total_sequence)/2)+1 <= Probe_Location[probe] <= (len(total_sequence)/2)+10:
            print('plop')
            difference = ((len(total_sequence)/2)+10) - Probe_Location[probe]
            Probe_Location[probe] = int(Probe_Location[probe] + difference)
Probe_Number = 0  # Initialize probe number
# %% 

##############   
# Experiment #
##############      
# ===== Experiment start =====
# %%  
date_start = datetime.datetime.now()  # Save the experiment's start time

# List of instruction texts
instruction_texts = [
    "Podczas tego eksperymentu zobaczysz obiekty pojawiające się na ekranie.\n\nWszystkie te obiekty będą miały zielony kolor oprócz jednego, który będzie w kolorze czerwonym.\n\nNaciśnij klawisz ”→” na klawiaturze, aby kontynuować.",

    "Na środku ekranu znajdować się będzie punkt fiksacji. Proszę utrzymuj na nim swój wzrok przez cały czas. \n\nTwoim celem będzie wskazanie, czy czerwony obiekt pojawi się na lewo, czy na prawo względem punktu fiksacji, używając strzałek kierunkowych w lewo i w prawo. Wszystkie obiekty będą wyświetlane przez bardzo krótki czas, a po chwili zostaną zasłonięte kolorowym ekranem.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",
    
    "Zadanie staraj się wykonywać tak szybko jak potrafisz, robiąc przy tym jak najmniej błędów.\n\nJeśli się pomylisz, na ekranie pojawi się napis „ERROR”.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",
    
    "Wspomniany wcześniej punkt fiksacji pojawi się przed prezentacją obiektów.\n\nMożesz wykorzystać ten moment, żeby zrelaksować oczy i zamknąć powieki.\n\nDo następnej próby przejdziesz dopiero jeśli wykryte zostanie dłuższe utrzymanie spojrzenia na krzyżyk.\n\nJeśli próba się nie uruchamia, postaraj się nie mrugać podczas patrzenia na krzyżyk.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",
    
    "Od czasu do czasu, w trakcie tego eksperymentu Twoje myśli odpłyną. Możesz myśleć wtedy na przykład o przyjemnym wspomnieniu, o tym co Ciebie czeka po ukończeniu eksperymentu, lub skupisz się na tym co obecnie fizycznie odczuwasz.\n\nPodczas eksperymentu, na ekranie pojawi się pytanie o stan skupienia Twojej uwagi. Odpowiedz w jakim stopniu byłeś/-aś skupiony/-a  na wykonaniu zadania tuż przed pojawieniem się tego pytania. Do zaznaczenia stanu swojego skupienia na skali, użyj myszki. \n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",

    "Wartości po lewej stronie skali oznaczają, że byłeś/ byłaś„w pełni skupiony na zadaniu” (co oznacza brak rozpraszających myśli, aktywne szukanie obiektu).\n\nWartości po prawej stronie skali oznaczją, że byłeś/-aś „w pełni rozproszony/nieskupiony” (co oznacza całkowite zanurzenie w myślach, wykonywanie zadania w sposób automatyczny).\n\nTo zupełnie normalne, aby myśleć o czymś innym podczas wykonywania eksperymentu, więc proszę odpowiadaj szczerze na pytanie.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’ENTER”, aby kontynuować."
]

current_page = 0
num_pages = len(instruction_texts)

while True:
    # Draw current instruction text
    Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=24, text=instruction_texts[current_page])
    Instructions.draw()
    win.flip()

    keys = event.waitKeys(keyList=['left', 'right', 'return', 'escape'])

    if 'right' in keys and current_page < num_pages - 1:
        current_page += 1
    elif 'left' in keys and current_page > 0:
        current_page -= 1
    elif 'return' in keys and current_page == num_pages - 1:
        break  # Exit after final screen

# Define pixel conversion for center positions
x_pix = deg_to_pix(fixation.pos[0], ExpMonitor)
y_pix = deg_to_pix(fixation.pos[1], ExpMonitor)
transformed_x_pix = x_pix + (screen_width / 2)
transformed_y_pix = abs(y_pix - (screen_height / 2))


# %%  

##############   
# Main Experiment Loop #
##############
# %%  
phase = ['Training','Experiment']
Block=1 
Trial_Block=1
n = 0
for step in phase:
    #print(step)
    # Experiment start
    if step == phase[0]:
        Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                       text='Naciśnij klawisz ’’ENTER” kiedy będziesz gotowy rozpocząć trening') 
        n_trial = training_trial
        
    elif step == phase[1]: 
        # Calibration (skip in dummy mode)
        if not DUMMY_MODE:
            calib_prompt = "Trening został zakończony.\n\nNaciśnij klawisz ’’ENTER”, aby rozpocząć."  # Calibration prompt message
            calib_msg = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, text=calib_prompt, color='white')  # Display message
            calib_msg.draw()  # Draw the calibration message
            win.flip()  # Refresh window
            el_tracker.doTrackerSetup()  # Start the EyeLink calibration
            event.clearEvents()
            el_tracker.setOfflineMode()  # Put tracker in idle mode
            el_tracker.startRecording(1,1,1,1)  # Start recording eye data
            pylink.msecDelay(100)  # Small delay for stabilization
            
        Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                       text='Kalibracja została zakończona.\n\nNaciśnij klawisz ’’ENTER” kiedy będziesz gotowy rozpocząć eksperyment.') 
        n_trial = len(total_sequence)
    Instructions.draw()  # Draw the instruction text
    win.flip()  # Refresh window
    event.waitKeys(keyList = ['return'])  # Wait for a keypress to continue


    
    el_tracker.sendMessage("DATA COLLECTION START") # Show start of experiment on edf file
    for i in range(n_trial):
        win.mouseVisible = False  # Hide mouse cursor during trials
        if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} START')  # Send trial ID to EyeLink
        el_tracker.sendCommand("record_status_message 'Trial running'")  # Show status on EyeLink
    
        pylink.pumpDelay(100)  # Short delay for tracker synchronization
        if step == phase[1]:el_tracker.sendMessage("STARTING RECORDING")  # Start recording
        core.wait(0.1)  # Wait for stable eye tracking
    
        if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} FIXATION_ONSET')  # Send message for fixation onset
        if expInfo['Test Mode'] == 'True':
            win.flip()  # Refresh window if in test mode
            
        else:
            fixation.draw()  # Display fixation cross
            win.flip()  # Refresh window
            mouse = event.Mouse(visible=True, win=win)  # Initialize mouse for interaction
            hover_start = None  # Variable for tracking hover start time
            while True:
                keys = event.getKeys(modifiers=True)  # Enable modifier key tracking
            
                key_names = [k[0] for k in keys]  # Extract only the key names (not modifiers)
                modifiers = keys[-1][1] if keys else {}  # Get the modifier status dict
            
                # Check for CTRL+ESCAPE
                if 'escape' in key_names and modifiers.get('shift', False):
                    win.close()
                    core.quit()
            
                # Check for CTRL+RETURN
                elif 'return' in key_names and modifiers.get('shift', False):
                    if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} FIXATION_MANUAL_QUIT')
                    break
                
                elif 'd' in key_names and modifiers.get('shift', False) and not DUMMY_MODE:
                    el_tracker.setOfflineMode()
                    pylink.msecDelay(50)
                    if step == phase[1]:el_tracker.sendMessage(f"MANUAL_DRIFT_CHECK {Probe_Number} START")
                    try:
                        el_tracker.doDriftCorrect(screen_width // 2, screen_height // 2, 1, 1)
                    except RuntimeError as err:
                        print('ERROR:', err)
                        el_tracker.exitCalibration()
                    
                    event.clearEvents(eventType='keyboard')
                    if step == phase[1]:el_tracker.sendMessage(f"MANUAL_DRIFT_CHECK {Probe_Number} END")
                    el_tracker.setOfflineMode()  # Put tracker in idle mode
                    el_tracker.startRecording(1,1,1,1)  # Start recording eye data
                    pylink.msecDelay(100)  # Small delay for stabilization

                    fixation.draw()  # Display fixation cross
                    win.flip()  # Refresh window
                    mouse = event.Mouse(visible=True, win=win)  # Initialize mouse for interaction
                    hover_start = None  # Variable for tracking hover start time
    
    
                if DUMMY_MODE:
                    if step == phase[0]:
                        mouse_x, mouse_y = 0,0
                    else:
                        mouse_x, mouse_y = mouse.getPos()  # Get mouse position (for dummy mode)
                    transformed_mouse_x = mouse_x + (screen_width / 2)
                    transformed_mouse_y = abs(mouse_y - (screen_height / 2))
                    #print(f"Target Position: ({transformed_x_pix}, {transformed_y_pix}), Mouse Position: ({transformed_mouse_x}, {transformed_mouse_y})")
                    if (transformed_x_pix - 60 <= transformed_mouse_x <= transformed_x_pix + 60) and (transformed_y_pix - 60 <= transformed_mouse_y <= transformed_y_pix + 60):
                        if hover_start is None:
                            hover_start = core.getTime()
                        elif core.getTime() - hover_start >= 1:
                            break  # Item disappears after 1000 ms of hovering
                    else:
                        hover_start = None
                else:
                    # EyeLink gaze tracking
                    win.mouseVisible = False  # Hide mouse cursor during trials
                    if step == phase[0]:
                        sample = 0
                    if step == phase[1]:
                        sample = el_tracker.getNewestSample()
                    #print(sample)

                    if sample is not None:
                        if step == phase[0]:
                            eye_sample = 0
                            gaze_x, gaze_y = screen_width/2,screen_height/2
                        else:
                            if expInfo['Dominant eye'] == 'Left':
                                eye_sample = sample.getLeftEye()
                            elif expInfo['Dominant eye'] == 'Right':
                                eye_sample = sample.getRightEye()
                        #print(step)
                        #print(gaze_x, gaze_y)
                        #print(transformed_x_pix)
                        #print(transformed_y_pix)
                        if eye_sample is not None and step == phase[1]:
                            gaze_x, gaze_y = eye_sample.getGaze()
                            #print(f"Eye position (PsychoPy coords): ({gaze_x}, {gaze_y})")  # Print gaze coordinates
                        if (transformed_x_pix - 60 <= gaze_x <= transformed_x_pix + 60) and (transformed_y_pix - 60 <= gaze_y <= transformed_y_pix + 60):
                            if hover_start is None:
                                hover_start = core.getTime()
                            elif core.getTime() - hover_start >= 1:
                                if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} FIXATION_END')  
                                break  # Item disappears after 1000 ms of hovering
                        else:
                            hover_start = None
            
        # Trial assignment (blocks, quadrants, etc.)
        Trial_Block+=1
        df["Block"][i] = Block
        df["Trial_Block"][i] = Trial_Block

        
        # Define quadrant positions for target presentation
        if step == phase[0]:
            Target_Quadrant = rd.randint(1, 4)

        else:
            Target_Quadrant = total_sequence[i]
        
        quadrant = Target_Quadrant
        position = rd.randint(1, 3)
        if quadrant == 1 and position ==1: angle = 115
        elif quadrant == 1 and position ==2: angle = 135
        elif quadrant == 1 and position ==3: angle = 155
        elif quadrant == 2 and position ==1: angle = 25
        elif quadrant == 2 and position ==2: angle = 45
        elif quadrant == 2 and position ==3: angle = 65
        elif quadrant == 3 and position ==1: angle = 205
        elif quadrant == 3 and position ==2: angle = 225
        elif quadrant == 3 and position ==3: angle = 245
        elif quadrant == 4 and position ==1: angle = 295
        elif quadrant == 4 and position ==2: angle = 315
        elif quadrant == 4 and position ==3: angle = 335
        else: angle =0
            
        # Create and draw each quadrant as a rectangle
        # for q in quadrants:
            # visual.Rect(win,width=hw,height=hh,
            #             pos=q["pos"],fillColor=q["color"],lineColor=q["color"],colorSpace='rgb255').draw()
            
        # Create target shape for the trial
        target = visual.Circle(win, units='deg', 
                               pos=[5*math.cos(math.radians(angle)), 5*math.sin(math.radians(angle))],
                               size=1, lineColor="#FF0000", fillColor="#FF0000")
        print(quadrant)
        print(position)
        # Save target's position and quadrant data to DataFrame
        df["Target_X"][i] = target.pos[0]  
        df["Target_Y"][i] = target.pos[1]  
        df["Target_Coordinates"][i] = target.pos  
        df["Target_Quadrant"][i] = Target_Quadrant
        if df["Target_Quadrant"][i] == 1:
            df["Target_Quadrant_Name"][i] = "Top-Left"
        elif df["Target_Quadrant"][i] == 2:
            df["Target_Quadrant_Name"][i] = "Top-Right"
        elif df["Target_Quadrant"][i] == 3:
            df["Target_Quadrant_Name"][i] = "Bottom-Left"
        elif df["Target_Quadrant"][i] == 4:
            df["Target_Quadrant_Name"][i] = "Bottom-Right"
        
        if df["Trial_Block"][i] > 1:
            df["Bigram"][i] = [total_sequence[i-1],total_sequence[i]]
            if Quadrant_Type[i] == "Expected":
                df["Bigram_Type"][i] = "High_Probability"
            else:
                df["Bigram_Type"][i] = "Low_Probability"
        else:
            df["Bigram"][i] = ''
            df["Bigram_Type"][i] = ''
                    
        # Save quadrant infos
        df["Quadrant_Type"][i] = Quadrant_Type[i]
    
        target.draw()  # Draw the target
        List_pos.append(target.pos)  # Append target position to list
    
        # Handle overlapping distractors (targets and distractors shouldn't overlap)
        Overlap = True
        Overlap_counter = 0
        Quadrant_N = 0
        quadrant = Target_Quadrant
        display_set = 3
        position = position+1
        if position > 3:
            position -= 3  # Ensure quadrant stays within range (1-4)
        
        # Loop for quadrant and distractor setup
        for Quadrant_N in range(4):
            if Quadrant_N == 0:
                Range = display_set-1
            else:
                Range = display_set
    
            x = 0    
            for distractor_n in range(Range):
                if quadrant == 1 and position ==1: angle = 115
                elif quadrant == 1 and position ==2: angle = 135
                elif quadrant == 1 and position ==3: angle = 155
                elif quadrant == 2 and position ==1: angle = 25
                elif quadrant == 2 and position ==2: angle = 45
                elif quadrant == 2 and position ==3: angle = 65
                elif quadrant == 3 and position ==1: angle = 205
                elif quadrant == 3 and position ==2: angle = 225
                elif quadrant == 3 and position ==3: angle = 245
                elif quadrant == 4 and position ==1: angle = 295
                elif quadrant == 4 and position ==2: angle = 315
                elif quadrant == 4 and position ==3: angle = 335
                else: angle =0
                
                x += 1
                while Overlap == True:  # Check for overlap of distractors with other objects
                    Overlap_counter = 0
                    
                    distractor = visual.Circle(win, units='deg', 
                                                  pos=[5*math.cos(math.radians(angle)), 5*math.sin(math.radians(angle))], 
                                                  size=1, lineColor="#00FF00", fillColor="#00FF00")
                    
                    
                    # Check if distractor overlaps any other object
                    # for pos in enumerate(List_pos):
                    #     if abs(distractor.pos[0] - pos[1][0]) < 2.5 and abs(distractor.pos[1] - pos[1][1]) < 2.5:
                    #         Overlap_counter += 1
                        
                    if Overlap_counter == 0:       
                        Overlap = False  # If no overlap, exit the loop
                distractor.draw()  # Draw the distractor
                List_pos.append(distractor.pos)  # Append the distractor's position to the list
                Overlap = True
                Overlap_counter = 0
                position+=1
                if position > 3:
                    position -= 3  # Ensure quadrant stays within range (1-4)
                
    
            quadrant += 1  # Move to the next quadrant
            position=1
            if quadrant > 4:
                quadrant -= 4  # Ensure quadrant stays within range (1-4)
        
        # Send message to EyeLink for display onset
        if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} Display_ONSET')
        win.mouseVisible = False  # Hide the mouse cursor
        fixation.draw()
        win.flip()  # Refresh the window
        
        # Set up reaction time (RT) clock for response recording
        Clock = core.Clock()
    
        # Test mode response generation (random response in test mode)
        if expInfo['Test Mode'] == 'True':
            Response = []  # Empty response list
            Response = [[(random.choice(["num_1", "num_2","num_4", "num_5"])), random.uniform(0, 1)]]  # Generate random response and RT
            df["Response"][i] = Response[0][0]  # Save the response
            df["RT"][i] = Response[0][1]  # Save the reaction time
            df["Trial"][i] = i + 1  # Save the trial number (+1 because indexing starts at 0)
    
        else:  # Participant's actual response
            Response = ''  # Empty response list
            Response = event.waitKeys(keyList=["num_1", "num_2","num_4", "num_5"], timeStamped=Clock,maxWait=100)  # Wait for participant's response
            if Response:
                print(Response[0][1])
                core.wait(0.1 - Response[0][1])
        
        # Generate new mask each trial
        mask_array = make_color_noise_mask(width=400, height=400,
            correlation=0.7,
            dark_threshold=100,
            pixel_size=20    # blocky, chunky color mask
        )
        
        mask = visual.ImageStim(win, image=mask_array, units="pix") # Create ImageStim
        
        if expInfo['Test Mode'] != 'True':
            mask.draw()
        win.flip()
        if not Response:
            Response = event.waitKeys(keyList=["num_1", "num_2","num_4", "num_5"], timeStamped=Clock,maxWait=0.2)  # Wait for participant's response
            if Response:
                print(Response[0][1])
                core.wait(0.2 - Response[0][1])
        else:
            if expInfo['Test Mode'] != 'True':
                core.wait(0.2)
        
        fixation.draw()
        win.flip()
        if not Response:
            Response = event.waitKeys(keyList=["num_1", "num_2","num_4", "num_5"], timeStamped=Clock,maxWait=7.5)  # Wait for participant's response
            print(Response[0][1])

        df["Response"][i] = Response[0][0]  # Save the response
        df["RT"][i] = Response[0][1]  # Save the reaction time
        if df["Response"][i] == "num_4": df["Response"][i] = "1" 
        elif df["Response"][i] == "num_5": df["Response"][i] = "2"
        elif df["Response"][i] == "num_1": df["Response"][i] = "3"
        elif df["Response"][i] == "num_2": df["Response"][i] = "4"

        df["Trial"][i] = i + 1  # Save the trial number

        # Check if the response matches the target's position
        if str(Target_Quadrant) == df["Response"][i]:
            df["Response_Correctness"][i] = 'Correct'  # If correct, store "Correct"
        else:
            if not df["Response"][i]:
                df["Response_Correctness"][i] = 'Miss'  # If incorrect, store "Incorrect"
                error_message = visual.TextStim(win, units="deg", pos=[0, +3], text='MISS !', color=(1, -1, -1))  # Error message
            else:
                df["Response_Correctness"][i] = 'Incorrect'  # If incorrect, store "Incorrect"
                error_message = visual.TextStim(win, units="deg", pos=[0, +3], text='ERROR !', color=(1, -1, -1))  # Error message
            if expInfo['Test Mode'] == 'True':
                core.wait(0)  # No wait if in test mode
            else:
                error_message.draw()  # Draw error message
                win.flip()  # Refresh window
                core.wait(0.5)  # Wait 500 ms for feedback
    
        # Send trial end message to EyeLink
        if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} END')
        win.flip()  # Refresh window
        List_pos = list()  # Reset list for positions
        
        # Probe loop for mind-wandering rating
        if Probe_Number < len(Probe_Location):
            if i == Probe_Location[Probe_Number] and step == phase[1] or i == training_trial-1 and step == phase[0] :
                mouse = event.Mouse(visible=True, win=win)  # Initialize mouse for probe interaction
                click_count = 0
                waiting_for_release = False  # Whether we are waiting for the release of a click
                first_click_value = None  # To store the first slider click value
                second_click_value = None  # To store the second slider click value
                Step = 0  # Step counter for click validation
                slider.markerPos = None  # Reset slider marker position
                
                # Reset the mouse state by setting its position (this clears internal state)
                mouse.setPos((0, 0))  # Set initial position of mouse
        
                # Clear any previous button press states
                while mouse.getPressed()[0]:  # Wait until the button is released
                    core.wait(0.01)  # Small delay to avoid busy-waiting
                
                if step == phase[1]:el_tracker.sendMessage(f"PROBEID {Probe_Number} START")  # Start probe
        
                # Draw probe loop
                while True:
                    Instructions1 = visual.TextStim(win, units='deg', pos=[0, +4], wrapWidth=20, text='Jak oceniasz stan swojego umysłu?')
                    Instructions1.draw()  # Draw instruction message
                    Instructions2 = visual.TextStim(win, units='deg', pos=[0, -6.5], wrapWidth=20, text='Kliknij lewy przycisk myszy, aby wybrać odpowiednią pozycję na suwaku i ponownie lewy przycisk, aby potwierdzić.', italic=True)
                    Instructions2.draw()  # Draw instruction message
                    slider.draw()  # Draw the slider
                    label_left.draw()  # Draw left label
                    label_right.draw()  # Draw right label
                    win.flip()  # Refresh window
        
                    # Get mouse position
                    mouse_x, mouse_y = mouse.getPos()  # Get mouse coordinates
        
                    # Check if mouse is over the slider
                    transformed_left_bound = deg_to_pix(left_bound, ExpMonitor)
                    transformed_right_bound = deg_to_pix(right_bound, ExpMonitor)
                    transformed_bottom_bound = deg_to_pix(bottom_bound, ExpMonitor)
                    transformed_top_bound = deg_to_pix(top_bound, ExpMonitor)
                    
                    # If the mouse is within the slider bounds, update slider color
                    if transformed_left_bound <= mouse_x <= transformed_right_bound and transformed_bottom_bound <= mouse_y <= transformed_top_bound:
                        slider.lineColor = "Yellow"  # Highlight slider when the mouse is over it
        
                        # Check for mouse click (left mouse button)
                        if mouse.getPressed()[0] and not waiting_for_release:
                            waiting_for_release = True  # Start waiting for the release
                        elif not mouse.getPressed()[0] and waiting_for_release:
                            waiting_for_release = False  # Reset waiting status for the next press
                            # Handle first and second click logic
                            if Step == 0:
                                first_click_value = slider.markerPos  # Store first click value
                                Step += 1  # Move to the next step
                            elif Step == 1:
                                second_click_value = slider.markerPos  # Store second click value
                                if first_click_value == slider.markerPos:  # Confirm if the second click is same
                                    Step += 1  # Exit the loop when both clicks are the same
                                else:
                                    first_click_value = slider.markerPos  # Reset the first click value
        
                        # Exit loop when both clicks are validated
                        if Step == 2:
                            break
                    else:
                        slider.lineColor = "White"  # Restore slider line color
                    
                    if expInfo['Test Mode'] == 'True':
                        slider.markerPos = rd.randint(1,10)
                        break
                
                if step == phase[1]:el_tracker.sendMessage(f"PROBEID {Probe_Number} END")  
        
                if not DUMMY_MODE and step == phase[1]:
                    el_tracker.setOfflineMode()
                    pylink.msecDelay(50)
                    el_tracker.sendMessage(f"DRIFT_CHECK {Probe_Number} START")
                    try:
                        el_tracker.doDriftCorrect(screen_width // 2, screen_height // 2, 1, 1)
                    except RuntimeError as err:
                        print('ERROR:', err)
                        el_tracker.exitCalibration()
                    
                    event.clearEvents(eventType='keyboard')
                    el_tracker.sendMessage(f"DRIFT_CHECK {Probe_Number} END")
                    el_tracker.setOfflineMode()  # Put tracker in idle mode
                    el_tracker.startRecording(1,1,1,1)  # Start recording eye data
                    pylink.msecDelay(100)  # Small delay for stabilization
                    
                if Probe_Number < len(Probe_Location) and step == phase[1]:  # If there are more probes, increment the probe number
                    Probe_Number += 1
                
                if step == phase[1]:
                    df["Probe_Number"][i] = Probe_Number  # Save the probe number
                    df["Probe_Response"][i] = slider.markerPos  # Save the probe response (mind-wandering rating)
                    Block+=1
                Trial_Block=0
                #print(df["Probe_Response"][i])
            if i == n_trial/2 and step == phase[1]:
                Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                            text='Ukończona została połowa eksperymentu.\n\nMożesz zrobić sobie chwilkę przerwy.\n\nNaciśnij dowolny klawisz, aby kontynuować.') 
                Instructions.draw()  # Draw the instruction text
                win.flip()  # Refresh window
                event.waitKeys()  # Wait for a keypress to continue
                
                # Calibration (skip in dummy mode)
                if not DUMMY_MODE:
                    calib_prompt = "Naciśnij klawisz ’’ENTER”, aby rozpocząć."  # Calibration prompt message
                    calib_msg = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, text=calib_prompt, color='white')  # Display message
                    calib_msg.draw()  # Draw the calibration message
                    win.flip()  # Refresh window
                    el_tracker.doTrackerSetup()  # Start the EyeLink calibration
                    event.clearEvents()
                    el_tracker.setOfflineMode()  # Put tracker in idle mode
                    el_tracker.startRecording(1,1,1,1)  # Start recording eye data
                    pylink.msecDelay(100)  # Small delay for stabilization
                    
                    Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                    text='Press any key to continue the experiment.') 
                    Instructions.draw()  # Draw the instruction text
                    win.flip()  # Refresh window
                    event.waitKeys()  # Wait for a keypress to continue
                
    
pylink.msecDelay(100)  # Small delay before stop recording
el_tracker.sendMessage("DATA COLLECTION END") # Show end of experiment on edf file
el_tracker.setOfflineMode()  # Set EyeLink to offline mode
core.wait(0.5)  # Wait for 500ms to catch session end events
# %%  

###############################   
# Post-experiment evaluations #
############################### 
# ===== Awareness assessment =====
# %%  
Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                text='Koniec głównego zadania.\n \nZawołaj eksperymentatora, aby udzielił Ci instrukcji do dalszych pytań.')
 
Instructions.draw()  # Draw the instruction text
win.flip()  # Refresh window
event.waitKeys()  # Wait for a keypress to continue

Instructions = visual.TextStim(win, units='deg', pos=[0, 0], wrapWidth=22, 
                                text='Mogliście zauważyć lub nie, że podczas eksperymentu powtarzała się określona sekwencja lokalizacji w której znajdował się szukany obiekt (jeśli tego nie zauważyliście, to w porządku). \n\nTeraz przetestujemy Waszą wiedzę na temat ćwiczonej sekwencji. Prosimy, abyście odtworzyli sekwencję lokalizacji w której znajdował się szukany obiekt, jaka pojawiała się podczas zadania. Używając klawiszy 1-4 wskaż, w której ćwiartce powinien pojawić się szukany obiekt.')  # Instructions for post-experiment evaluation

Instructions.draw()  # Draw instruction message
win.flip()  # Refresh window
event.waitKeys()  # Wait for a keypress to continue

Instructions = visual.TextStim(win, units='deg', pos=[0, 0], wrapWidth=22, 
                                text='Proszę kontynuować wskazywanie spodziewanego położenia obiektu, aż poprosimy abyście przestali. \n\nJeśli nie jesteście pewni następnej lokalizacji, spróbujcie polegać na swojej intuicji i odpowiedzieć najlepiej, jak potraficie. \n\nNaciśnij ENTER, aby kontynuować')  # Instructions for post-experiment evaluation

Instructions.draw()  # Draw instruction message
win.flip()  # Refresh window
event.waitKeys(keyList = ['return'])
# Define quadrant positions for post-experiment evaluation
quad_size_pix = deg_to_pix(8.5, ExpMonitor)  # Define quadrant size in pixels
quadrant_positions = {
    "top-left": (-quad_size_pix / 2, quad_size_pix / 2),  
    "top-right": (quad_size_pix / 2, quad_size_pix / 2),
    "bottom-left": (-quad_size_pix / 2, -quad_size_pix / 2),
    "bottom-right": (quad_size_pix / 2, -quad_size_pix / 2),
}

# Quadrant labels and inward offsets
labels = {"top-left": "1", "top-right": "2", "bottom-left": "3", "bottom-right": "4"}
offset = quad_size_pix * 0.4
offsets = {
    "top-left": ( offset, -offset),
    "top-right": (-offset, -offset),
    "bottom-left": ( offset,  offset),
    "bottom-right": (-offset,  offset),
}

# Create quadrant rectangles with transparent fill and white borders
quadrants = {
    name: visual.Rect(win, width=quad_size_pix, height=quad_size_pix, fillColor=None, lineColor="white", pos=pos, units='pix')
    for name, pos in quadrant_positions.items()
}
labels_stim = {
    name: visual.TextStim(win, text=labels[name],
                          pos=(quadrant_positions[name][0] + offsets[name][0],
                               quadrant_positions[name][1] + offsets[name][1]),
                          color='white', height=50, units='pix')
    for name in labels
}

Response_Explicit = []
Response_Implicit = []

for awareness_task in ["Explicit", "Implicit"]:
    if awareness_task == "Implicit":
        instr = visual.TextStim(win, units='deg', pos=[0, 0], wrapWidth=22,
                                text='Teraz prosimy, abyście odtworzyli sekwencję lokalizacji w której znajdował się szukany obiekt w odwrotnej kolejności. To znaczy, prosimy wskazać, która lokalacja była najbardziej prawdopodobna przed każdą inną. Na przykład, jeśli uważacie, że kolejność była a-b-c, powinniście wskazać c-b-a. \n\nJeśli nie jesteście pewni następnej lokalizacji, spróbujcie polegać na swojej intuicji i odpowiedzieć najlepiej, jak potraficie. \n\nNaciśnij ENTER, aby kontynuować')
        instr.draw()
        win.flip()
        event.waitKeys(keyList=['return'])
        
    seq = 32
    Response = []
    for n in range(seq):
        if n == 0:
            for name in quadrants:
                quadrants[name].draw()
                labels_stim[name].draw()
            win.flip()

        key = event.waitKeys(keyList=["num_4", "num_5", "num_1", "num_2"])[0]
        Response.append(key)

        selected = {
            "num_4": "top-left",
            "num_5": "top-right",
            "num_1": "bottom-left",
            "num_2": "bottom-right"
        }[key]
        x, y = quadrant_positions[selected]

        for name in quadrants:
            quadrants[name].draw()
            labels_stim[name].draw()

        visual.Circle(win, pos=(x, y), radius=20, edges=1000,
                      fillColor='red', lineColor='red', units='pix').draw()
        win.flip()

    if awareness_task == "Explicit":
        for response in Response:
            if response == "num_4": Response_Explicit.append(1)
            elif response == "num_5": Response_Explicit.append(2)
            elif response == "num_1": Response_Explicit.append(3)
            elif response == "num_2": Response_Explicit.append(4)
    else:
        for response in Response:
            if response == "num_4": Response_Implicit.append(1)
            elif response == "num_5": Response_Implicit.append(2)
            elif response == "num_1": Response_Implicit.append(3)
            elif response == "num_2": Response_Implicit.append(4)
    
# %%  

# ===== Global Mind Wandering Assessment =====
# %%  
# Display instruction message for percentage input
prompt = visual.TextStim(win, units='deg', pos=[0, 3], wrapWidth=20,
                         text="Określ jaki procent czasu Twoje myśli były rozproszone podczas eksperymentu?\n\n"
                         "Wpisz za pomocą klawiatury liczbę od 0 do 100 i naciśnij ’’ENTER‘’, aby potwierdzić.")

response_display = visual.TextStim(win, units='deg', pos=[0, -2], text="0%", height=1.5, color="white")

percentage = ""  # Stores typed input
loop = True  # Loop control variable

while loop:  # Infinite loop until a valid response is given
    prompt.draw()  # Draw the prompt message
    response_display.text = percentage + "%" if percentage else "0%"  # Display current input
    response_display.draw()  # Draw input display
    win.flip()  # Refresh window

    # Get pressed keys
    keys = event.getKeys()

    for key in keys:
        if key == "return" and percentage.isdigit() and 0 <= int(percentage) <= 100:
            df["MW_Percentage"] = int(percentage)  # Store response
            event.clearEvents()  # Prevent PsychoPy from hanging
            loop = False  # Exit loop when valid input is received
        elif key == "backspace":  # Allow correction of input
            percentage = percentage[:-1]
        elif key in "0123456789" and len(percentage) < 3:  # Allow numeric input (up to 3 digits)
            percentage += key
        elif key in ["0", "1", "2", "3", "4",  
                     "5", "6", "7", "8", "9"] and len(percentage) < 3:
            percentage += key[-1]  # Extract the last digit from the input key

    if "return" in keys and percentage.isdigit() and 0 <= int(percentage) <= 100:
        break  # Exit the loop when a valid response is given
# %%  

########################    
# End of the experiment #
########################
# %%  
win.flip()  # Refresh the window
end_message = visual.TextStim(win, units="deg", pos=[0, +3], text='Koniec eksperymentu.\n\nNaciśnij dowolny klawisz, aby kontynuować.')  # End message
end_message.draw()  # Draw end message
win.flip()  # Refresh window
event.waitKeys()  # Wait for a keypress to finish the experiment
# %%  

# ===== Behavioral data save =====
# %%  
# Save behavioral data to DataFrame and CSV
df["Initials"] = expInfo['Initials']  # Save participant's initials
df["Codename"] = edf_filename  # Save participant's codename
df["Age"] = expInfo['Age']  # Save participant's age
df["Gender"] = expInfo['Gender']  # Save participant's gender
df["Sequence"] = pd.Series([sequence] * len(df))  # Apply the entire sequence list in each cell
df["Dominant_Eye"] = expInfo['Dominant eye']  # Save participant's dominant eye
df["Date"] = expInfo['Date']  # Save experiment date
df["Test_Mode"] = expInfo['Test Mode']  # Save test mode setting
df["Sequence_awareness_Explicit"] = pd.Series([Response_Explicit] * len(df))  # Apply the entire response list in each cell
df["Sequence_awareness_Implicit"] = pd.Series([Response_Implicit] * len(df))  # Apply the entire response list in each cell
df["Experiment_End"] = datetime.datetime.now()  # Save the experiment's end time
df["Experiment_Start"] = date_start  # Save the experiment's start time
df["Total_Duration"] = (df["Experiment_End"] - date_start).total_seconds()  # Calculate total duration of the experiment

# Convert the dictionary to a DataFrame
df = pd.DataFrame.from_dict(df)

os.chdir(csv_path)  # Change directory for saving behavioral data
if len(Data_list) > 0:
    if len(Data_list) < 9:
        df.to_csv("0" + str(len(Data_list)+1) + expInfo['Initials'] + str(expInfo['Age']) + expInfo['Gender'] + ".csv")  # Save data to CSV
    else:
        df.to_csv(str(len(Data_list)+1) + expInfo['Initials'] + str(expInfo['Age']) + expInfo['Gender'] + ".csv")  # Save data to CSV
else:
    df.to_csv("0" + str(len(Data_list)+1) + expInfo['Initials'] + str(expInfo['Age']) + expInfo['Gender'] + ".csv")  # Save data to CSV
# %%  

# ===== Eye tracker data save =====
# %%  
# Save eye tracker data (EDF format) and convert to ASCII
win.flip()  # Refresh the window
end_message = visual.TextStim(win, units="deg", pos=[0, +3], text='Zapisywanie danych...')  # End message
end_message.draw()  # Draw end message
win.flip()  # Refresh window
os.chdir(edf_path)
pylink.msecDelay(100)  # Small delay before saving data
el_tracker.closeDataFile()  # Close the data file
el_tracker.receiveDataFile(f'{edf_filename}.edf', os.path.join(edf_path, f'{edf_filename}.edf'))  # Save EDF file to disk
subprocess.run(["edf2asc", f'{edf_filename}.edf'])  # Convert EDF to ASCII format
el_tracker.close()  # Close the EyeLink tracker
pylink.closeGraphics()  # Close EyeLink graphics interface

win.flip()  # Refresh the window
end_message = visual.TextStim(win, units="deg", pos=[0, +3], text='Dziękujemy za udział w badaniu!\n\nNaciśnij dowolny klawisz, aby zakończyć eksperyment.')  # End message
end_message.draw()  # Draw end message
win.flip()  # Refresh window

event.waitKeys()  # Wait for a keypress to finish the experiment
win.close()  # Close PsychoPy window
core.quit()  # End the experiment
# %%  
