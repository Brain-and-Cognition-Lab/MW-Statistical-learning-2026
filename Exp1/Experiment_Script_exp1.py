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
from scipy import stats as stats  # For statistical functions, e.g., random uniform distribution
import random as rd # For generating random numbers
from itertools import permutations  # For generating all permutations of quadrant sequences
import pandas as pd  # For managing data in DataFrame format
import datetime  # For working with dates and times
import tkinter as tk  # For GUI elements like input dialogs
import os as os  # For interacting with the file system
import math  # For mathematical functions (e.g., tan, radians)
# %% 

# ===== Definitions =====
# %%  
def deg_to_pix(deg, monitor):
    """Convert degrees of visual angle to pixels."""  
    return tan(deg2rad(deg)) * monitor.getDistance() * (monitor.getSizePix()[0] / monitor.getWidth())  # Convert degrees to pixels based on screen size and distance

def deg_to_cm(deg, distance_cm):
    """Convert degrees of visual angle to centimeters given a viewing distance."""
    return math.tan(math.radians(deg)) * distance_cm  # Multiply tangent of angle (in radians) by viewing distance
# %% 

# ===== Base setup =====
# %%  
# Path setup  
path = "C:/Enter/Your/Path/Here" 
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
training_trial = 10  # Number of trials in the training phase
# %% 

# ===== Dataframe setup =====
# %%  
# Data dictionary to store all experiment data for each trial  
df = {"Initials": {}, "Codename": {},"Age": {}, "Gender": {}, "Sequence": {}, "Dominant_Eye": {}, "Block": {}, "Test_Mode": {}, "Trial": {}, "Trial_Block": {}, 
      "Target_Direction": {}, "Target_Coordinates": {}, "Target_Quadrant": {}, "Target_Quadrant_Name": {},
      "Target_X": {}, "Target_Y": {}, "Quadrant_Type": {},"Bigram": {},"Bigram_Type": {}, "Response": {}, "Response_Correctness": {},
      "Probe_Number": {}, "Probe_Response": {}, "RT": {}, "Experiment_Start": {}, "Experiment_End": {}, "Total_Duration": {}}
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
w, h = win.size  # Retrieve window dimensions in pixels
hw, hh = w // 2, h // 2  # Compute half-width and half-height for quadrant positioning

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
label_left = visual.TextStim(win, text="Zupełnie\nskupiony", pos=(-7.5, -2), units="deg", color="White", wrapWidth=6)  # Left anchor label ("Fully focused")
label_right = visual.TextStim(win, text="Zupełnie\nrozproszony", pos=(7.5, -2), units="deg", color="White", wrapWidth=6)  # Right anchor label ("Fully distracted")

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

all_perms = list(permutations([1, 2, 3, 4]))  # Generate all 24 permutations of quadrant indices

# Remove ascending and descending sequences (all rotations)
patterns = [
    (1,2,3,4), (2,3,4,1), (3,4,1,2), (4,1,2,3),  # ascending
    (4,3,2,1), (3,2,1,4), (2,1,4,3), (1,4,3,2),   # descending
    (1,2,4,3), (2,4,3,1), (4,3,1,2), (3,1,2,4),   # clockwise
    (4,2,1,3), (2,1,3,4), (1,3,4,2), (3,4,2,1)   # counterclockwise
]

sequence = list(rd.choice([p for p in all_perms if p not in patterns]))  # Randomly select a valid sequence (excluding structured patterns)

total_sequence = []  # Full trial-by-trial sequence of target quadrants
for i in range(600):
    if i == 0:
        total_sequence.append(rd.randint(1,4))  # First trial: random quadrant
        Quadrant_Type.append("")  # No type for the first trial
    else:
        current_num = total_sequence[-1]  # Last quadrant in the sequence
        current_index = sequence.index(current_num)  # Index of current quadrant in the chosen sequence
        next_in_sequence = sequence[(current_index + 1) % 4]  # Next expected quadrant (wraps around)
        
        if rd.random() < 0.7:
            total_sequence.append(next_in_sequence)  # 70% chance: follow the expected sequence
            Quadrant_Type.append("Expected")
        else:
            options = [x for x in [1,2,3,4] if x not in (current_num, next_in_sequence)]  # Remaining quadrants (excluding current and expected)
            total_sequence.append(rd.choice(options))  # 30% chance: pick an unexpected quadrant
            Quadrant_Type.append("Unexpected")


List_pos = list()  # List to store target positions
# %%  

# ===== Probe setup =====
# %%  
# Create probe locations for mind-wandering probes (20 probes placed at random intervals)
Probe_Location = []
for probe in range(20):
    if Probe_Location == []:
        Probe_Location.append(random.randint(round(len(total_sequence) / 20)-3, round(len(total_sequence) / 20)+3))  # First probe: near the first interval
    elif probe == 9:
        Probe_Location.append(int(len(total_sequence)/2)-1)  # 10th probe: placed at the midpoint of the experiment
    elif probe == 19:
        Probe_Location.append(int(len(total_sequence))-1)  # Last probe: placed at the final trial
    else:
        Probe_Location.append(Probe_Location[probe-1] + random.randint(round(len(total_sequence) / 20)-3, round(len(total_sequence) / 20)+3))  # Subsequent probes: spaced ~evenly with jitter
        if (len(total_sequence)/2)+1 <= Probe_Location[probe] <= (len(total_sequence)/2)+10:
            difference = ((len(total_sequence)/2)+10) - Probe_Location[probe]  # Compute offset to avoid clustering around midpoint
            Probe_Location[probe] = int(Probe_Location[probe] + difference)  # Shift probe away from midpoint buffer zone
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
    "Podczas tego eksperymentu zobaczysz obiekty pojawiające się na ekranie.\n\nWszystkie te obiekty będą miały kształt litery ’’L” oprócz jednego, który będzie przybierał  kształt\n⊣ lub ⊢.\n\nNaciśnij klawisz ”→” na klawiaturze, aby kontynuować.",

    "Twoim celem jest zauważenie wyróżniającego się obiektu i odpowiednie zareagowanie na jego położenie poprzez naciśnięcie odpowiedniego klawisza na klawiaturze numerycznej. \n\n„1” jeśli obiekt pojawi się w lewym górnym rogu, „2” jeśli w prawym górnym rogu, „3” jeśli w lewym dolnym rogu, lub „4” jeśli w prawym dolnym rogu. Zadanie staraj się wykonywać tak szybko jak potrafisz, robiąc przy tym jak najmniej błędów.\n\nJeśli się pomylisz, na ekranie pojawi się napis „ERROR”.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",
  
    "Przed każdą próbą, pojawi się krzyżyk na środku ekranu.\n\nMożesz wykorzystać ten moment, żeby zrelaksować oczy i zamknąć powieki.\n\nDo następnej próby przejdziesz dopiero jeśli wykryte zostanie dłuższe utrzymanie spojrzenia na krzyżyk.\n\nJeśli próba się nie uruchamia, postaraj się nie mrugać podczas patrzenia na krzyżyk.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",
  
    "Od czasu do czasu, w trakcie tego eksperymentu Twoje myśli odpłyną. Możesz myśleć wtedy na przykład o przyjemnym wspomnieniu, o tym co Ciebie czeka po ukończeniu eksperymentu, lub skupisz się na tym co obecnie fizycznie odczuwasz.\n\nPodczas eksperymentu, na ekranie pojawi się pytanie o stan skupienia Twojej uwagi. Odpowiedz w jakim stopniu byłeś/-aś skupiony/-a  na wykonaniu zadania tuż przed pojawieniem się tego pytania. Aby odpowiedzieć na to pytanie, użyj myszki do zaznaczenia na skali stan swojego skupienia. \n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’→”, aby kontynuować.",

    "Wartości po lewej stronie skali oznaczają, że byłeś/ byłaś„w pełni skupiony na zadaniu” (co oznacza brak rozpraszających myśli, aktywne szukanie obiektu).\n\nWartości po prawej stronie skali oznaczją, że byłeś/-aś „w pełni rozproszony/nieskupiony” (co oznacza całkowite zanurzenie w myślach, wykonywanie zadania w sposób automatyczny).\n\nTo zupełnie normalne, aby myśleć o czymś innym podczas wykonywania eksperymentu, więc proszę odpowiadaj szczerze na pytanie.\n\nUżyj klawiszy ’’←’’ aby wrócić, lub ’’ENTER”, aby kontynuować."
]

current_page = 0  # Index of the currently displayed instruction page
num_pages = len(instruction_texts)  # Total number of instruction pages

while True:
    # Draw current instruction text
    Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=24, text=instruction_texts[current_page])
    Instructions.draw()
    win.flip()

    keys = event.waitKeys(keyList=['left', 'right', 'return', 'escape'])

    if 'right' in keys and current_page < num_pages - 1:
        current_page += 1  # Advance to the next instruction page
    elif 'left' in keys and current_page > 0:
        current_page -= 1  # Go back to the previous instruction page
    elif 'return' in keys and current_page == num_pages - 1:
        break  # Exit after final screen

# Define pixel conversion for center positions
x_pix = deg_to_pix(fixation.pos[0], ExpMonitor)  # Convert fixation x-position from degrees to pixels
y_pix = deg_to_pix(fixation.pos[1], ExpMonitor)  # Convert fixation y-position from degrees to pixels
transformed_x_pix = x_pix + (screen_width / 2)  # Shift origin from center to top-left (screen coordinates)
transformed_y_pix = abs(y_pix - (screen_height / 2))  # Flip y-axis to match screen coordinate system


# %%  

##############   
# Main Experiment Loop #
##############
# %%  
phase = ['Training','Experiment']
Block=1  # Block counter, incremented after each mind-wandering probe
Trial_Block=1  # Trial counter within the current block, reset after each probe
n = 0  # General-purpose counter (currently unused in the loop body)
for step in phase:
    # Experiment start
    if step == phase[0]:
        Instructions = visual.TextStim(win, units='deg', pos=[0,0], wrapWidth=20, 
                                       text='Naciśnij klawisz ''ENTER" kiedy będziesz gotowy rozpocząć trening') 
        n_trial = training_trial  # Set number of trials to training length
        
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
                                       text='Kalibracja została zakończona.\n\nNaciśnij klawisz ''ENTER" kiedy będziesz gotowy rozpocząć eksperyment.') 
        n_trial = len(total_sequence)  # Set number of trials to full experiment length
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
                        mouse_x, mouse_y = 0,0  # In training dummy mode, simulate gaze at screen center
                    else:
                        mouse_x, mouse_y = mouse.getPos()  # Get mouse position (for dummy mode)
                    transformed_mouse_x = mouse_x + (screen_width / 2)  # Convert to screen coordinates
                    transformed_mouse_y = abs(mouse_y - (screen_height / 2))  # Flip y-axis for screen coordinates
                    if (transformed_x_pix - 60 <= transformed_mouse_x <= transformed_x_pix + 60) and (transformed_y_pix - 60 <= transformed_mouse_y <= transformed_y_pix + 60):
                        if hover_start is None:
                            hover_start = core.getTime()  # Start timing the hover
                        elif core.getTime() - hover_start >= 1:
                            break  # Item disappears after 1000 ms of hovering
                    else:
                        hover_start = None  # Reset hover timer if gaze leaves fixation area
                else:
                    # EyeLink gaze tracking
                    win.mouseVisible = False  # Hide mouse cursor during trials
                    if step == phase[0]:
                        sample = 0  # No real sample needed during training
                    if step == phase[1]:
                        sample = el_tracker.getNewestSample()  # Retrieve the most recent gaze sample

                    if sample is not None:
                        if step == phase[0]:
                            eye_sample = 0  # Placeholder for training phase
                            gaze_x, gaze_y = screen_width/2,screen_height/2  # Default gaze to screen center during training
                        else:
                            if expInfo['Dominant eye'] == 'Left':
                                eye_sample = sample.getLeftEye()  # Use left eye data
                            elif expInfo['Dominant eye'] == 'Right':
                                eye_sample = sample.getRightEye()  # Use right eye data

                        if eye_sample is not None and step == phase[1]:
                            gaze_x, gaze_y = eye_sample.getGaze()  # Extract gaze coordinates
                        if (transformed_x_pix - 60 <= gaze_x <= transformed_x_pix + 60) and (transformed_y_pix - 60 <= gaze_y <= transformed_y_pix + 60):
                            if hover_start is None:
                                hover_start = core.getTime()  # Start timing fixation on cross
                            elif core.getTime() - hover_start >= 1:
                                if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} FIXATION_END')  
                                break  # Item disappears after 1000 ms of hovering
                        else:
                            hover_start = None  # Reset hover timer if gaze leaves fixation area
            
        # Trial assignment (blocks, quadrants, etc.)
        Trial_Block+=1
        df["Block"][i] = Block
        df["Trial_Block"][i] = Trial_Block

        
        # Define quadrant positions for target presentation
        if step == phase[0]:
            Target_Quadrant = rd.randint(1, 4)  # Random quadrant during training

        else:
            Target_Quadrant = total_sequence[i]  # Follow pre-generated sequence during experiment
        if Target_Quadrant == 1:
            loc_x = -7.5  # Top-left quadrant: x origin in degrees
            loc_y = 1.5   # Top-left quadrant: y origin in degrees
        elif Target_Quadrant == 2:
            loc_x = 1.5   # Top-right quadrant: x origin in degrees
            loc_y = 1.5   # Top-right quadrant: y origin in degrees
        elif Target_Quadrant == 3:
            loc_x = -7.5  # Bottom-left quadrant: x origin in degrees
            loc_y = -7.5  # Bottom-left quadrant: y origin in degrees
        elif Target_Quadrant == 4:
            loc_x = 1.5   # Bottom-right quadrant: x origin in degrees
            loc_y = -7.5  # Bottom-right quadrant: y origin in degrees
            
        # Create and draw each quadrant as a rectangle
        for q in quadrants:
            visual.Rect(win,width=hw,height=hh,
                        pos=q["pos"],fillColor=q["color"],lineColor=q["color"],colorSpace='rgb255').draw()
            
        # Create target shape for the trial
        target = visual.ShapeStim(win, units='deg', 
                                  pos=[stats.uniform.rvs(loc=loc_x, scale=6), stats.uniform.rvs(loc=loc_y, scale=6)],  # Random position within the target quadrant
                                  vertices=((-1, 1), (-1, -1), (-1, 0), (1, 0)),  # T-shaped target (⊣ or ⊢ depending on orientation)
                                  lineWidth=4, closeShape=False, lineColor="white")
    
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
            df["Bigram"][i] = [total_sequence[i-1],total_sequence[i]]  # Store the current quadrant pair (bigram)
            if Quadrant_Type[i] == "Expected":
                df["Bigram_Type"][i] = "High_Probability"  # Expected transition = high-probability bigram
            else:
                df["Bigram_Type"][i] = "Low_Probability"  # Unexpected transition = low-probability bigram
        else:
            df["Bigram"][i] = ''  # No bigram for the first trial in a block
            df["Bigram_Type"][i] = ''  # No bigram type for the first trial in a block
        
        Flip = random.choice(["left", "right"])  # Randomly choose whether the target is flipped or not
        
        # Apply flip based on random choice
        if Flip == "left":
            target.ori = 180  # Rotate 180° to produce ⊢ orientation
        else:
            target.ori = 0  # Keep default orientation (⊣)
            
        # Save quadrant infos
        df["Target_Direction"][i] = Flip  # Save target orientation (left/right flip)
        df["Quadrant_Type"][i] = Quadrant_Type[i]  # Save whether this trial was Expected or Unexpected
    
        target.draw()  # Draw the target
        List_pos.append(target.pos)  # Append target position to list
    
        # Handle overlapping distractors (targets and distractors shouldn't overlap)
        Overlap = True  # Flag to indicate whether a distractor overlaps with existing stimuli
        Overlap_counter = 0  # Counter for the number of overlapping objects
        Quadrant_N = 0  # Current quadrant index in the distractor loop
        quadrant = Target_Quadrant  # Start distractor placement from the target's quadrant
        
        # Loop for quadrant and distractor setup
        for Quadrant_N in range(4):
            if Quadrant_N == 0:
                Range = 2  # Target quadrant gets 2 distractors
            else:
                Range = 3  # Other quadrants get 3 distractors each
    
            if quadrant == 1:
                loc_x = -7.5
                loc_y = 1.5
            elif quadrant == 2:
                loc_x = 1.5
                loc_y = 1.5
            elif quadrant == 3:
                loc_x = -7.5
                loc_y = -7.5
            elif quadrant == 4:
                loc_x = 1.5
                loc_y = -7.5
    
            x = 0    
            for distractor_n in range(Range):
                x += 1  # Increment distractor counter within the current quadrant
                while Overlap == True:  # Check for overlap of distractors with other objects
                    Overlap_counter = 0
                    distractor = visual.ShapeStim(win, units='deg', 
                                                  pos=[stats.uniform.rvs(loc=loc_x, scale=6), stats.uniform.rvs(loc=loc_y, scale=6)], 
                                                  vertices=rd.choice([((-1,1), (-1,-1), (-1,0.8), (1,0.8)),   # L-shape variant 1
                                                                         ((-1,1), (-1,-1), (-1,-0.8), (1,-0.8)),  # L-shape variant 2
                                                                         ((1,1), (1,-1), (1,0.8), (-1,0.8)),      # L-shape variant 3
                                                                         ((1,1), (1,-1), (1,-0.8), (-1,-0.8))]),  # L-shape variant 4
                                                  lineWidth=4, closeShape=False, lineColor="white")
                    # Check if distractor overlaps any other object
                    for pos in enumerate(List_pos):
                        if abs(distractor.pos[0] - pos[1][0]) < 2.5 and abs(distractor.pos[1] - pos[1][1]) < 2.5:
                            Overlap_counter += 1  # Increment counter if overlap detected
                        
                    if Overlap_counter == 0:       
                        Overlap = False  # If no overlap, exit the loop
                distractor.draw()  # Draw the distractor
                List_pos.append(distractor.pos)  # Append the distractor's position to the list
                Overlap = True  # Reset overlap flag for the next distractor
                Overlap_counter = 0  # Reset overlap counter for the next distractor
    
            quadrant += 1  # Move to the next quadrant
            if quadrant > 4:
                quadrant -= 4  # Ensure quadrant stays within range (1-4)
        
        # Send message to EyeLink for display onset
        if step == phase[1]:el_tracker.sendMessage(f'TRIALID {i} Display_ONSET')
        win.mouseVisible = False  # Hide the mouse cursor
        win.flip()  # Refresh the window
        
        # Set up reaction time (RT) clock for response recording
        Clock = core.Clock()
    
        # Test mode response generation (random response in test mode)
        if expInfo['Test Mode'] == 'True':
            Response = []  # Empty response list
            Response = [[(random.choice(["1","2","3","4"])), random.uniform(0, 1.5)]]  # Generate random response and RT
            df["Response"][i] = Response[0][0]  # Save the response
            df["RT"][i] = Response[0][1]  # Save the reaction time
            df["Trial"][i] = i + 1  # Save the trial number (+1 because indexing starts at 0)
    
        else:  # Participant's actual response
            Response = event.waitKeys(keyList=["num_1", "num_2","num_4", "num_5"], timeStamped=Clock)  # Wait for participant's response
            df["Response"][i] = Response[0][0]  # Save the response
            if df["Response"][i] == "num_4": df["Response"][i] = "1"  # Remap numpad key to quadrant label
            elif df["Response"][i] == "num_5": df["Response"][i] = "2"  # Remap numpad key to quadrant label
            elif df["Response"][i] == "num_1": df["Response"][i] = "3"  # Remap numpad key to quadrant label
            elif df["Response"][i] == "num_2": df["Response"][i] = "4"  # Remap numpad key to quadrant label
            df["RT"][i] = Response[0][1]  # Save the reaction time
            df["Trial"][i] = i + 1  # Save the trial number
    
            # Check if the response matches the target's position
        if str(Target_Quadrant) == df["Response"][i]:
            df["Response_Correctness"][i] = 'Correct'  # If correct, store "Correct"
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
                click_count = 0  # Counter for mouse clicks on the slider
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
                    Instructions1 = visual.TextStim(win, units='deg', pos=[0, +4], wrapWidth=20, text='Jak oceniasz stan swojego umysłu?')  # Probe question ("How do you rate your mental state?")
                    Instructions1.draw()  # Draw instruction message
                    Instructions2 = visual.TextStim(win, units='deg', pos=[0, -6.5], wrapWidth=20, text='Kliknij lewy przycisk myszy, aby wybrać odpowiednią pozycję na suwaku i ponownie lewy przycisk, aby potwierdzić.', italic=True)  # Click instruction
                    Instructions2.draw()  # Draw instruction message
                    slider.draw()  # Draw the slider
                    label_left.draw()  # Draw left label
                    label_right.draw()  # Draw right label
                    win.flip()  # Refresh window
        
                    # Get mouse position
                    mouse_x, mouse_y = mouse.getPos()  # Get mouse coordinates
        
                    # Check if mouse is over the slider
                    transformed_left_bound = deg_to_pix(left_bound, ExpMonitor)  # Convert left bound to pixels
                    transformed_right_bound = deg_to_pix(right_bound, ExpMonitor)  # Convert right bound to pixels
                    transformed_bottom_bound = deg_to_pix(bottom_bound, ExpMonitor)  # Convert bottom bound to pixels
                    transformed_top_bound = deg_to_pix(top_bound, ExpMonitor)  # Convert top bound to pixels
                    
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
                    Block+=1  # Increment block counter after each probe
                Trial_Block=0  # Reset trial-within-block counter
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

########################    
# End of the experiment #
########################
# %%  
end_message = visual.TextStim(win, units="deg", pos=[0, +3], text='Koniec eksperymentu.\n\nNaciśnij dowolny klawisz, aby kontynuować.')  # End message
end_message.draw()  # Draw end message
win.flip()  # Refresh window
event.waitKeys()  # Wait for a keypress to finish the experiment
# %%  

# ===== Behavioural data save =====
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
