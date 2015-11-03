'''
Created on Jun 15, 2015

@author: hratliff
'''

import math
import os
import sys
import datetime
import re
import numpy as np
import ctypes
from tkinter import filedialog, Tk

"""
from math import log, exp, sqrt
from os.path import splitext, dirname
from sys import stderr, __stderr__
from datetime import datetime
from re import split
from numpy import array, zeros, copy, delete, ndenumerate, mean, nonzero
from numpy.linalg import inv
# from tkinter import filedialog, Tk
"""

'''
This Python script is based on the original CLSQ code written in Fortran predating F77

~~~~~ NOTES ~~~~~
These notes are referred to in some of the comments in this code.  This helps keep the code less cluttered with comment text, especially with repeated comments.
A majority of variable names are unchanged from the original code. Some meaning behind the naming conventions are listed here.
- Variables ending in "SQ" are likely just squared versions of another variable.
- EOB in a variable name implies relation to the time at end of bombardment.
- TO in a variable name implies relation to T0 or the starting time.
- Variables that start with S or SIG or SIGMA are likely related to error analysis.
- The H and HL variables contain the half-life data inputed by the user in string format.
- The D and DL variables contain the decay constant values in min^-1 calculated from the H and HL variables.
- KCS in a variable name implies relation to the known component subtraction section of code.
- EXP in a variable name means the variable is equal to an exponential.
- Variables starting with "goto" are used as on/off switches for loop control to emulate the many GOTO statements in the original code.
- The NON variable is just a weird way to control whether "CONVERGENT" or "NONCONVERGENT" is printed to the output file.

1.) I do not know what exactly this variable is for.  It was in the original CLSQ and has been ported over to this version too.
'''

"""
# Input variable meanings
# Control card
NC = 1      # Number of components
NV = 1      # Number of unknown half-lives
NCNV = 0    # ? legacy users
CNV = 0.05  # Governs how far the iterations will proceed.  Iterate until CNV < change in decay constant / standard deviation of decay constant.
BGD = 0     # Counter background for individual background subtraction
SBGD = 5.0  # Background standard deviation
IN = 1      # 0=CLSQ input format, 1=read in from input deck
IT = 0      # Normally left blank, used to check if matrix inversion worked properly (B^-1 * B = U)
BLOCK = 5   # Counter dead time in microseconds
SCOFF = 0.5 # Set cutoff percent the standard deviation will never fall beneath
RJT = 6.0   # Bad point rejection.  If not 0, causes the program to reject points that fall under RJT times the standard deviation from the curve
KCS = 0     # Known component subtraction (leave at 0 for now, usually blank)
YIELD = 0   # ? Legacy users
"""

# Debug mode
# If turned on, the code will execute some print statements to help describe what is going on.
debug_mode = 0  # 1 = on, 0 = off

# Read in from text file.
Tk().withdraw() # we don't want a full GUI, so keep the root window from appearing
path_to_input = filedialog.askopenfilename(title="Select input file") # show an "Open" dialog box and return the path to the selected file, initialdir="C:\CLSQ\CLSQ2"

# If not using tkinter:
#path_to_input = input("\nPlease enter the input filename with extension and press Enter.\n\n")

filename, fileextension = os.path.splitext(path_to_input)
directory_name = os.path.dirname(path_to_input)
output_filename = filename + ".out"
error_log_filename = directory_name + "/CLSQ2_error_log.txt"

print_errors_to_file = 1  # 0 = print to terminal, 1 = print to external file
# If using the actual Python code instead of the .EXE, it may be desirable to set this = 0 so errors appear in the terminal instead.
if print_errors_to_file == 1: sys.stderr = open(error_log_filename, 'w')

f = open(output_filename,"w+")

size_limit_small = 22  # Default limit for number of components + 2
size_limit_large = 200 # Default limit for number of data points
iteration_limit = 20 # Default limit for iterating

FKCS = []
SFKCS = []

# Half life data
H = []
HL = []    
CEF = [] 
TBOMT= []  

# Tabular data
days = []
hours = []
minutes = []
date = []
counts = []
count_time = []
BGND = []
SBGND = []
SIGPCT = []
TYPE_1 = []
EP = []
IDENT_1 = []
EOB=""

# Input file debugging functions

def check_for_colon(i, line):
    if ":" in line.rsplit(None, 1)[1]:
        ctypes.windll.user32.MessageBoxW(0, "Place space between colon and entry on line number " + str(i+1) + " of input file.", "Input Error", 0)
        f.write("Error: Place space between colon and entry on line number " + str(i+1) + " of input file.")

def check_for_time_units(i, line):
    temp_text = ((str(re.split(r'[;,\s]\s*', line.strip())[0])))
    time_units = set("sSmMhHdDyY")
    if not any(substring in temp_text for substring in time_units): 
        ctypes.windll.user32.MessageBoxW(0, "Error: Make sure there is no space between number and time character in line " + str(i+1) + " of input file.", "Input Error", 0)
        f.write("Error: Make sure there is no space between number and time character in line " + str(i+1) + " of input file.")


num_time_entries = 3 # number of time entries, default of 3 = D H M format

# Read in input file

fp = open(path_to_input)
for i, line in enumerate(fp):
    if i == 0:
        # 1st line
        top_comment = line
    elif i == 1:
        # 2nd line
        check_for_colon(i, line)
        NC = (int(line.rsplit(None, 1)[1])) # Number of components
    elif i == 2:
        # 3rd line
        check_for_colon(i, line)
        NV = (int(line.rsplit(None, 1)[1])) # Number of unknown half-lives
    elif i == 3:
        # 4th line
        check_for_colon(i, line)
        NCNV = (int(line.rsplit(None, 1)[1]))    
    elif i == 4:
        # 5th line
        check_for_colon(i, line)
        CNV = (float(line.rsplit(None, 1)[1])) # Iteration stop condition (ratio: change in decay constant divided by standard deviation of decay constant)
    elif i == 5:
        # 6th line
        check_for_colon(i, line)
        BGD = (float(line.rsplit(None, 1)[1]))
    elif i == 6:
        # 7th line
        check_for_colon(i, line)
        SBGD = (float(line.rsplit(None, 1)[1]))
    elif i == 7:
        # 8th line
        check_for_colon(i, line)
        IN = (int(line.rsplit(None, 1)[1]))
    elif i == 8:
        # 9th line
        check_for_colon(i, line)
        IT = (int(line.rsplit(None, 1)[1]))
    elif i == 9:
        # 10th line
        check_for_colon(i, line)
        BLOCK = (float(line.rsplit(None, 1)[1]))
    elif i == 10:
        # 11th line
        check_for_colon(i, line)
        SCOFF = (float(line.rsplit(None, 1)[1]))
    elif i == 11:
        # 12th line
        check_for_colon(i, line)
        RJT = (float(line.rsplit(None, 1)[1]))
    elif i == 12:
        # 13th line
        temp_line = line.split(':', 1)[-1].strip()
        temp_line = re.split(r'[;,\s]\s*', temp_line)
        KCS = int(temp_line[0])
        #KCS = int(re.split(r'[;,\s]\s*', temp_line)[0])
        #KCS = (float(line.rsplit(None, 1)[1]))
        for j in range(KCS):
            FKCS.append(0.0)
            SFKCS.append(0.0)
            if len(temp_line) > j+1:
                FKCS[j] = float(temp_line[j+1])
                #FKCS[j] = (float(re.split(r'[;,\s]\s*', temp_line)[i+1]))
            if len(temp_line) > j+KCS+1:
                SFKCS[j] = float(temp_line[j+KCS+1])
                #SFKCS[j] = (float(re.split(r'[;,\s]\s*', temp_line)[i+int(KCS)+1]))
    elif i == 13:
        # 14th line
        YIELD_1 = (float(line.rsplit(None, 1)[1]))
    elif i >= 15 and i<=15+NC-1:
        # 16th line through all t_1/2 specifications
        #print(re.split(r'[;,\s]\s*', line.strip()))
        H.append("")
        HL.append(0.0)
        CEF.append(0.0)
        TBOMT.append(0.0)
        check_for_time_units(i, line)
        H[i-15] = ((str(re.split(r'[;,\s]\s*', line.strip())[0])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > 1:
            HL[i-15] = ((str(re.split(r'[;,\s]\s*', line.strip())[1])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > 2:
            CEF[i-15] = ((float(re.split(r'[;,\s]\s*', line.strip())[2])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > 3:
            TBOMT[i-15] = ((float(re.split(r'[;,\s]\s*', line.strip())[3])))
    elif i == 15+NC:
        # Need to determine the time syntax being used
        temp_line = line.split(':', 1)[-1].strip()   # obtain everything right of first colon
        
        # Determine what syntax is being used
        time_syntax = 0
        date_info = 0
        time_info = 0
        # 0 = raw number in minutes (default)
        # 1 = 0.0D "standard time input"
        # 2 = MM/DD/YY
        # 3 = MM/DD/YY HH:MM
        # 4 = MM/DD/YY HH:MM:SS

        
        time_units = set("sSmMhHdDyY")
        if any(substring in temp_line for substring in time_units):
            # Time is given in standard format
            EOB = (str(line.rsplit(None, 1)[1])) 
            time_syntax = 1           
                    
        if "/" in temp_line and ":" in temp_line:
            # Date mode with hours and minutes.  Determine if seconds used too
            if temp_line.count(":") == 1:
                temp_line = re.split(r'[;,\s]\s*', temp_line)  # turn temp_line into a list containing the time pieces
                temp_line = temp_line[0] + " " + temp_line[1]  # reformat it in an exact way
                # minutes only
                time_syntax = 3
                num_time_entries = 2
                EOB_date = datetime.datetime.strptime(temp_line, "%m/%d/%Y %H:%M")
            elif temp_line.count(":") == 2:
                temp_line = re.split(r'[;,\s]\s*', temp_line)  # turn temp_line into a list containing the time pieces
                temp_line = temp_line[0] + " " + temp_line[1]  # reformat it in an exact way
                # minutes and seconds
                time_syntax = 4
                num_time_entries = 2
                EOB_date = datetime.datetime.strptime(temp_line, "%m/%d/%Y %H:%M:%S")
                
        elif "/" in temp_line:
            # Date only
            temp_line = re.split(r'[;,\s]\s*', temp_line)  # turn temp_line into a list containing the time pieces
            time_syntax = 2
            num_time_entries = 1
            EOB_date = datetime.datetime.strptime(temp_line[0], "%m/%d/%Y")
        
        if time_syntax == 0:
            EOB = (float(line.rsplit(None, 1)[1])) 
        
        
    elif i >= 15+NC+4 and line.strip() != "" :
        if time_syntax == 0 or time_syntax == 1:
            days.append((float(re.split(r'[;,\s]\s*', line.strip())[0])))
            hours.append((float(re.split(r'[;,\s]\s*',line.strip())[1])))
            minutes.append((float(re.split(r'[;,\s]\s*',line.strip())[2])))
        elif time_syntax == 2:
            date.append(datetime.datetime.strptime(re.split(r'[;,\s]\s*',line.strip())[0], "%m/%d/%Y"))
        elif time_syntax == 3:
            temp_date = re.split(r'[;,\s]\s*',line.strip())[0] + " " + re.split(r'[;,\s]\s*',line.strip())[1]
            date.append(datetime.datetime.strptime(temp_date, "%m/%d/%Y %H:%M"))
        elif time_syntax == 4:
            temp_date = re.split(r'[;,\s]\s*',line.strip())[0] + " " + re.split(r'[;,\s]\s*',line.strip())[1]
            date.append(datetime.datetime.strptime(temp_date, "%m/%d/%Y %H:%M:%S"))
        
        counts.append((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries])))
        count_time.append((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 1])))
        BGND.append(0.0)
        SBGND.append(0.0)
        SIGPCT.append(0.0)
        TYPE_1.append("")
        EP.append(0.0)
        IDENT_1.append("")
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 2:
            BGND[i-(15+NC+4)] = ((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 2])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 3:
            SBGND[i-(15+NC+4)] = ((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 3])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 4:
            SIGPCT[i-(15+NC+4)] = ((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 4])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 5:
            TYPE_1[i-(15+NC+4)] = (((re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 5])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 6:
            EP[i-(15+NC+4)] = ((float(re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 6])))
        if len(re.split(r'[;,\s]\s*', line.strip())) > num_time_entries + 7:
            IDENT_1[i-(15+NC+4)] = (((re.split(r'[;,\s]\s*',line.strip())[num_time_entries + 7])))
fp.close()


# Adjust matrix size limits if needed.
if (NC+NV) > size_limit_small :
    size_limit_small = NC + NV  # Default limit for number of components
    iteration_limit = size_limit_small + 2

if len(counts) > size_limit_large :
    size_limit_large = len(counts) + 10 # Default limit for number of data points


'''
# Uncomment to check if input is being processed correctly
print(NC)
print(NV)
print(CNV)
print(BGD)
print(SIGMA)
print(IN)
print(IT)
print(BLOCK)
print(SCOFF)
print(RJT)
print(KCS)
print(H)
print(EOB)
print(days)
print(hours)
print(minutes)
print(counts)
print(count_time)
'''

# Convert all time inputs into minutes

# Half Lives H_num, Decay constant D
H_num = [] # half life list in minutes
D = [] # decay constant (lambda) in min^-1
for i in range(len(H)):
    raw_number  = float(H[i][:-1])
    if H[i].endswith('S') or H[i].endswith('s'):
        H_num.append(raw_number/60)
    elif H[i].endswith('M') or H[i].endswith('m'):
        H_num.append(raw_number)
    elif H[i].endswith('H') or H[i].endswith('h'):
        H_num.append(raw_number*60)
    elif H[i].endswith('D') or H[i].endswith('d'):
        H_num.append(raw_number*60*24)
    elif H[i].endswith('Y') or H[i].endswith('y'):
        H_num.append(raw_number*60*24*365.25)
    D.append(math.log(2)/H_num[i])
        
# Half Lives HL_num, Decay constant DL
HL_num = [] # half life list in minutes
DL = [] # decay constant (lambda) in min^-1
for i in range(len(HL)):
    if bool(HL[i]):
        raw_number  = float(HL[i][:-1])
        if HL[i].endswith('S') or HL[i].endswith('s'):
            HL_num.append(raw_number/60)
        elif HL[i].endswith('M') or HL[i].endswith('m'):
            HL_num.append(raw_number)
        elif HL[i].endswith('H') or HL[i].endswith('h'):
            HL_num.append(raw_number*60)
        elif HL[i].endswith('D') or HL[i].endswith('d'):
            HL_num.append(raw_number*60*24)
        elif HL[i].endswith('Y') or HL[i].endswith('y'):
            HL_num.append(raw_number*60*24*365.25)
        DL.append(math.log(2)/HL_num[i])
     
# End of Bombardment time conversion 
if time_syntax == 0:
    EOB_min = EOB
elif time_syntax == 1:
    EOB_raw_number = float(EOB[:-1])
    if EOB.endswith('S') or EOB.endswith('s'):
        EOB_min = (EOB_raw_number/60)
    elif EOB.endswith('M') or EOB.endswith('m'):
        EOB_min = (EOB_raw_number)
    elif EOB.endswith('H') or EOB.endswith('h'):
        EOB_min = (EOB_raw_number*60)
    elif EOB.endswith('D') or EOB.endswith('d'):
        EOB_min = (EOB_raw_number*60*24)
    elif EOB.endswith('Y') or EOB.endswith('y'):
        EOB_min = (EOB_raw_number*60*24*365.25)
    if debug_mode == 1 : print("EOB (minutes) = ", EOB_min)

# Do all math and conversions for count and time data
time_min = []
#decay_constant = []
CPM=[]  # counts per minute
for i in range(len(counts)):
    # Time data for measurements
    if time_syntax == 0 or time_syntax == 1:
        time_min.append(minutes[i]+(60*hours[i])+(24*60*days[i])+(count_time[i]/2))
    # Calculate decay constants (lambda)
    #decay_constant.append(math.log(2)/time_min[i])
    # Convert count data to counts per minute
    if count_time != 0:
        CPM.append(counts[i]/count_time[i])
    elif count_time == 0:
        CPM.append(0.0)

#print(time_min)
CPM_np = np.array(CPM)

# Create text to print to output file
output_text = ""
f.write("Output for: " +top_comment + "\n")
f.write("CLSQ2 output creation time: " + str(datetime.datetime.now()) + "\n")
# Column headings for output file
f.write('\n ----- INPUT DATA FROM FILE ----- \n\n')
if time_syntax == 0 or time_syntax == 1:
    f.write('   DAY    HR       MIN     COUNTS        DELTA_T           BGND   SBGND   SIGPCT               CPM      TYPE-FWHM  ENERGY       ID1       ID2  \n')
elif time_syntax >= 2 or time_syntax <= 4:
    f.write('          DATE/TIME        COUNTS        DELTA_T           BGND   SBGND   SIGPCT               CPM      TYPE-FWHM  ENERGY       ID1       ID2  \n')

# Translation of CLSQ code
# This looks messy because the original code was messy and this is just a translation of it.
# The Fortran GOTO statements have been turned into equivalent if statements.
LT1 = 0      # I think this maybe means "less than one."  It seems to be used as an if statement argument later.
TBOM = 0.0   # See Note 1.
DFACT = 0.0  # See Note 1.
FACT1 = np.zeros(size_limit_small)   # See Note 1.
FACT2 = np.zeros(size_limit_small)   # See Note 1.

# Lines 40 through 73 of original source code
for i in range(len(H)):
    if bool(TBOMT[i]):
        TBOM = float(TBOMT[i])
    if bool(HL[i]):
        DFACT = DL[i]/(DL[i]-D[i])
        FACT1[i] = float(CEF[i]) + DFACT
        FACT2[i] = -1*DFACT
        
        if TBOM == 0:
            FACT2[i] = FACT2[i]/FACT1[i]
        elif TBOM != 0:
            EXP1 = math.exp(-D[i]*TBOM)
            EXP2 = math.exp(-DL[i]*TBOM)
            GFACT = ((1.0-EXP2)-DFACT*(EXP1-EXP2))/(1.0-EXP1)
            FACT2[i] = FACT2[i] + GFACT
            
    elif not bool(HL[i]):
        FACT2[i] = 0

#print(FACT1)
#print(FACT2)
NP = 0  # Number of time data points provided

# Input option with IN variable
if IN == 0:
    print("Other input formats not supported. IN variable will be set to 2.")
    IN = 2
    
# At this point, the original code called the DATAIN subroutine.
# As seen in the earlier code, all input has already been read in.
B = np.zeros((size_limit_small,size_limit_small))
EL = np.zeros((size_limit_small,size_limit_small))
F = CPM_np.copy()  # background corrected counts
SFSQ = np.zeros(int(size_limit_large+1))

# Calculations done while data was read in.
for i in range(len(counts)):
    if IN == 1:
        # NP = NP + 1
        print("Other input formats not supported. IN variable will be set to 2.")
        IN = 2
    elif IN != 2 and IN != 1:
        print("Other input formats not supported. IN variable will be set to 2.")  
        IN = 2
    
    if IN >= 2:
        # Format the columns in the output that restate the input
        if time_syntax == 0 or time_syntax == 1:
            line_text = "{0:5.0f}\t{1:4.0f}\t{2:6.2f}\t{3:9.0f}\t{4:8.1f}\t{5:7.2f}\t{6:7.2f}\t{7:8.1F}\t{8:10.2f}\t{9: <8} {10:8.1f} {11: <8}\n".format(days[i],hours[i],minutes[i],counts[i],count_time[i],float(BGND[i]),float(SBGND[i]),float(SIGPCT[i]),CPM[i],TYPE_1[i],float(EP[i]),IDENT_1[i])
        elif time_syntax >= 2 or time_syntax <= 4:
            line_text = "{0:s}\t{1:9.0f}\t{2:8.1f}\t{3:7.2f}\t{4:7.2f}\t{5:8.1F}\t{6:10.2f}\t{7: <8} {8:8.1f} {9: <8}\n".format(str(date[i]),counts[i],count_time[i],float(BGND[i]),float(SBGND[i]),float(SIGPCT[i]),CPM[i],TYPE_1[i],float(EP[i]),IDENT_1[i])
        f.write(line_text)
        NP = NP + 1
        
    if count_time[i] != 0:
        #F[i] = CPM[i] # background corrected counts
        
        if bool(SIGPCT[i]):
            SFSQ[i] = ((SIGPCT[i]/100)*F[i])**2
        elif not bool(SIGPCT[i]):
            SFSQ[i] = (counts[i]+1)/(count_time[i]**2)
        
        if BGD < 0:
            F[i] = F[i] - BGND[i]
            SFSQ[i] = SFSQ[i] + (SBGND[i]**2)
    
f.write("\n\n ----- START OF CALCULATED OUTPUT ----- \n")

# Time normalization
time_adjusted = []
if time_syntax == 0 or time_syntax == 1:
    T0 = EOB_min  # set initial time to end of bombardment
    for i in range(NP):
        time_adjusted.append(time_min[i]-EOB_min)
    
    T0 = time_adjusted[0]  # initial time should be amount of time passed between EOB and 1st measurement
elif time_syntax >= 2 or time_syntax <= 4 :
    for i in range(NP):
        temp_seconds = (date[i] - EOB_date).total_seconds()
        time_adjusted.append(temp_seconds/60)
    T0 = time_adjusted[0]  # initial time should be amount of time passed between EOB and 1st measurement

# Dead time correction
if BLOCK != 0:
    DEADT = BLOCK*(1.67E-8)  # 1.67E-8 minutes = 1 microsecond
    for i in range(NP):
        F[i] = F[i]/(1.0 - (DEADT*F[i]))

if BGD > 0:
    SBGDSQ=SBGD**2
    for i in range(NP):
        F[i] = F[i] - BGD
        SFSQ[i] = SFSQ[i] + SBGDSQ
        
if YIELD_1 != 0:
    YLDSQ = YIELD_1**2
    for i in range(NP):
        F[i] = F[i]/YIELD_1
        SFSQ[i] = SFSQ[i]/YLDSQ
        
# Sigma cutoff
if SCOFF != 0:
    SCOFSQ = (SCOFF**2)/10000
    for i in range(NP):
        TSFSQ = SCOFSQ*(F[i]**2)
        if SFSQ[i] < TSFSQ:
            SFSQ[i] = TSFSQ

# Known component subtraction (KCS)
if KCS != 0:
    for j in range(1,KCS+1):
        ND = NC - KCS + j
        for i in range(1,NP+1):
            FACTOR_1 = math.exp((T0-time_adjusted[i-1])*D[ND-1])
            F[i-1] = F[i-1] - (FKCS[j-1]*FACTOR_1)
            SFSQ[i-1] = SFSQ[i-1] + (SFKCS[j-1]*FACTOR_1)**2
    NC = NC - KCS

# Label 47 in original code
# Initialization and Returns
NITER = -2  # started at -1 in original code.  Starting at -2 here since Python indices start at 0.
if NV != 0:
    LT1 = 1
    NVTEMP = NV
    NV = 0
# elif NV == 0:

# This is the earliest destination of any GOTO label (that makes sense)
# Thus, since GOTO and labels don't exist in Python, a while loop with if statements will be used.
master_loop_criteria = 1
goto51 = 1
goto77 = 0

# Declare arrays that will be used in loop
# The size limits are arbitrary and could be increased.
# This is simply how the matrices were declared in the original code.
A = np.zeros((size_limit_large,size_limit_small))
APF = np.zeros(size_limit_small)
B = np.zeros((size_limit_small,size_limit_small))
X = np.zeros(size_limit_small)
DELTAD = np.zeros((size_limit_small,int(size_limit_small-1)))
SIGMAD = np.zeros(size_limit_small)
SIGMAH = np.zeros(size_limit_small)
EXPDTO = np.zeros(size_limit_small)   # exp(D*T0), D = decay constant
XEOB   = np.zeros(size_limit_small)
SXEOB  = np.zeros(size_limit_small)
FCALC  = np.zeros(size_limit_large)
V      = np.zeros(size_limit_large)
SF     = np.zeros(size_limit_large)
RATIO_1= np.zeros(size_limit_large)
BADT   = np.zeros(size_limit_large)
BADFS  = np.zeros(size_limit_large)

while master_loop_criteria == 1:
        
    if goto51 == 1:
        # Label 51 in original code
        # Return after fixed half life pass
        NT = NC + NV
        LV = NC + 1 
        if debug_mode == 1 : print("NT = ",NT,", NC = ",NC,", NV = ",NV, ", LV = ",LV) 
        
        #B_to_inv = np.zeros((NT,NT))
        # Declare arrays that will be used in loop
        """
        A = np.zeros((NP,NC))
        APF = np.zeros(NT)
        B = np.zeros((NT,NT))
        X = np.zeros(NT)
        DELTAD = np.zeros((10,9))
        SIGMAD = np.zeros(10)
        SIGMAH = np.zeros(10)
        EXPDTO = np.zeros(10)
        XEOB   = np.zeros(10)
        SXEOB  = np.zeros(10)
        FCALC  = np.zeros(200)
        V      = np.zeros(200)
        SF     = np.zeros(200)
        RATIO_1= np.zeros(200)
        BADT   = np.zeros(200)
        BADFS  = np.zeros(200)
        """
    
    goto51 = 0 # Prevents label 51 up to 77 from being executed again unless called for
    
    # Iteration loops
    repeat_condition = 1
    while repeat_condition == 1:
        if goto77 != 1:
            # Label 55 in original code
            # Calculation of A matrix
            NITER = NITER + 1
            for i in range(NP):
                for j in range(NC):
                    DT = (-1*D[j])*(time_adjusted[i]-T0)
                    if DT <= -34.5 :
                        A[i,j] = 0
                    elif DT > -34.5 :
                        A[i,j] = math.exp(DT)
                    if (HL[j] != 0) and (FACT2[j] != 0) :
                        DLT = -1*DL[j]*time_adjusted[i]
                        if DLT >= -34.5 :
                            A[i,j] = A[i,j] + (FACT2[j]*math.exp(DLT)*math.exp(D[j]*T0))
                
                if NV > 0 :
                    for jj in range(LV,NT+1):
                        k = jj - NC
                        A[i,jj-1] = (time_adjusted[i]-T0)*A[i,k-1]
                        #print("A[",i,",",jj-1,"] = ",A[i,jj-1])
        
        # Label 77 in original code
        # Construct matrix APF
        for i in range(NT):
            APF[i] = 0.0
            for k in range(NP):
                APF[i] = APF[i] + (A[k,i]*F[k]/SFSQ[k])
                
        # Construct matrix B
        for i in range(NT):
            for j in range(NT):
                B[i,j] = 0.0
                for k in range(NP):
                    B[i,j] = B[i,j] + (A[k,i]*A[k,j]/SFSQ[k])
                    #B_to_inv[i,j] = B[i,j]  # Normal B matrix will not inverse properly with its excess zeros
        
       
       
        # B matrix inversion
        # In the original code, this was a whole subroutine
        # It was able to process all of the extra zeros in the matrix
        # Unfortunately, this trick no longer works in matrix inversion routines
        # So, temporarily, B is trimmed of extra zeros for inversing.  Zeros are then added back to the BINVRS matrix.
        """B_to_inv = (B[~np.all(B == 0, axis = 1)])
        B_to_inv = (B[:,~np.all(B == 0, axis = 0)])"""
        numrows = len(B)
        numcolumns = len(B[0])
        B_to_inv = np.copy(B)
        for iii in range(numrows-1,0,-1):
            if B_to_inv[iii].sum() == 0:
                B_to_inv = np.delete(B_to_inv, (iii), axis = 0)
        for jjj in range(numcolumns-1,0,-1):
            if B_to_inv[:,jjj].sum() == 0:
                B_to_inv = np.delete(B_to_inv, (jjj), axis = 1)
        
        #print(B_to_inv)
        BINVRS = np.linalg.inv(B_to_inv)
        B2 = np.zeros((size_limit_small,size_limit_small))
        for (xx,yy), value in np.ndenumerate(BINVRS):
            B2[xx,yy] = BINVRS[xx,yy]
        BINVRS = B2
        
        
        # Solution of normal equations
        for i in range(NT):
            X[i] = 0.0
            for j in range(NT):
                X[i] = X[i] + (BINVRS[i,j]*APF[j])
                #print("APF[",j,"] = ",APF[j])
                
        # Entrance to calculations for Output or Iteration tests
        if NV == 0:  # If NV=0, GO TO 127
            repeat_condition = 0
            break
        elif NV != 0:  
            NON = 0 # NO = 0, YES = 1
            for i in range(NV):  
                L = i + NC
                if debug_mode == 1 : print("NITER = ", NITER, ", and L = i + NC = ", i, " + ", NC, " = ", L)
                DELTAD[i,NITER] = -X[L]/X[i]
                
                while abs(DELTAD[i,NITER]) >= (0.5*abs(D[i])):
                    # Change in the decay constant cannot exceed half its value
                    DELTAD[i,NITER] = 0.75*DELTAD[i,NITER]
                
                D[i] = D[i] + DELTAD[i,NITER]
            
            if NITER <= 1:
                continue  # return to label 55, top of while loop
            
            
            if NCNV <= 0 :
                exit_condition = 0 # must stay = to 0 or code would exit DO loop
                for i in range(NV):
                    if (abs(DELTAD[i,NITER]) > abs(DELTAD[i,NITER-1])) and (abs(DELTAD[i,NITER-1]) > abs(DELTAD[i,NITER-2])):
                        exit_condition = 1
                if exit_condition == 0:
                    NON = 1 # Yes
            elif NCNV > 0 :
                NON = 1 # Yes
                
            
            for i in range(NV):
                L = i + NC
                sqrt_argument = ( (BINVRS[i,i]/(X[i]**2)) + (BINVRS[L,L]/(X[L]**2)) - (2.0*BINVRS[i,L]/(X[i]*X[L])))
                if debug_mode == 1 : print("L = ",L," NT = ",NT)
                if debug_mode == 1 : print("Taking SQRT of ", sqrt_argument)
                
                if sqrt_argument >= 0 :
                    SIGMAD[i] = abs(DELTAD[i,NITER])*math.sqrt(sqrt_argument)
                elif sqrt_argument < 0 :
                    print("Taking SQRT of ", sqrt_argument, " , NEGATIVE NUMBER!  Taking absolute value.  Do not trust results.")
                    f.write("Tried to take square root of {0:12.4f}.  Since it was negative, absolute value was taken. \n Therefore, DO NOT TRUST THESE RESULTS!\n".format(sqrt_argument))
                    sqrt_argument = abs(sqrt_argument)
                    SIGMAD[i] = abs(DELTAD[i,NITER])*math.sqrt(sqrt_argument)
                if debug_mode == 1 : print("X[L] = ",X[L])
            
            if (NON == 0) or (NITER >= iteration_limit):
                repeat_condition = 0
                break
            
            if (NON != 0) and (NITER < iteration_limit):
                repeat_condition_2 = 0
                for i in range(NV):
                    if abs(DELTAD[i,NITER]) >= (CNV*SIGMAD[i]):
                        repeat_condition_2 = 1
                        if NITER >= iteration_limit:  # I don't think this condition will ever be met and don't know why it was in the original code
                            NON = 0
                if repeat_condition_2 == 1:
                    repeat_condition = 1
                elif repeat_condition_2 == 0:
                    repeat_condition = 0
                    break
    
    
    
    # Label 127 in original code
    # Calculations for output data
    for i in range(NC):
        SIGMAH[i] = 0.0
        EXPDTO[i] = math.exp(D[i]*T0)
    
    # Half life output conversion
    #H_out = []
    if NV >= 0: # was originally if NV != 0.  I don't know why this conversion would not be performed when NV = 0.
        for i in range(NC+KCS):  # was to NV in original code, but that doesn't seem right since we need to do time conversions on all t_1/2 before printing
            # Revert units back to match user input
            if H[i].endswith('S') or H[i].endswith('s'):
                H_num[i] = ( ((math.log(2))*60) / D[i] )
                #H_out.append( ((math.log(2))*60) / D[i] )
            elif H[i].endswith('M') or H[i].endswith('m'):
                H_num[i] = ( ((math.log(2))) / D[i] )
                #H_out.append( ((math.log(2))) / D[i] )
            elif H[i].endswith('H') or H[i].endswith('h'):
                H_num[i] = ( ((math.log(2))/60) / D[i] )
                #H_out.append( ((math.log(2))/60) / D[i] )
            elif H[i].endswith('D') or H[i].endswith('d'):
                H_num[i] = ( ((math.log(2))/60/24) / D[i] )
                #H_out.append( ((math.log(2))/60/24) / D[i] )
            elif H[i].endswith('Y') or H[i].endswith('y'):
                H_num[i] = ( ((math.log(2))/60/24/365.25) / D[i] )
                #H_out.append( ((math.log(2))/60/24/365.25) / D[i] )
                
            """print(H[i])
            print(SIGMAD[i])
            print(D[i])
            print(X)
            print(APF)
            print(SIGMAD)"""
            SIGMAH[i] = H_num[i]*SIGMAD[i]/D[i]
            #SIGMAH[i] = H_out[i]*SIGMAD[i]/D[i]
    
    # Label 211 in original code
    # Print out results
    f.write("\n\n\n")
    single_vals = "NP = {0:d} ,  NC = {1:d} ,  NV = {2:d} ,  CNV = {3:4.2f} ,  BGD = {4:6.2f} ,  SBGD = {5:5.2f} ,  BLOCK = {6:4.1f} ,  SCOFF = {7:3.1f} ,  RJT = {8:4.1F} ,  KCS = {9:d} ,  YIELD = {10:6.3f} \n".format(NP,NC,NV,CNV,BGD,SBGD,BLOCK,SCOFF,RJT,KCS,YIELD_1)
    f.write(single_vals)
    f.write("\n")
    
    if NV != 0 :
        if NON == 0:
            NONt = "NON"
        elif NON == 1:
            NONt = "   "
        f.write("Iterations performed = {0:2d} ,  {1:s}CONVERGENT \n".format(NITER+1, NONt))
        f.write("\n\n")
        f.write("              1ST COMP        2ND COMP        3RD COMP        4TH COMP        5TH COMP \n")
        f.write("    D       ")
        for i in range(NV):
            #print(D, i, D[i])
            f.write("{0:14.6E}\t".format(D[i]))
        f.write(" \n")
        
        #print(DELTAD)
        for j in range(NITER+1):
            f.write("  DELTA({0:2d}) ".format(j+1))
            for i in range(NV):
                #print("(i,j) = (", i, ",",j,")")
                f.write("{0:14.6E}\t".format(DELTAD[i,j]))
            f.write(" \n")
        
        f.write("  SIGMA     ")
        for i in range(NV):
            f.write("{0:14.6E}\t".format(SIGMAD[i]))
        f.write("\n")
        f.write("\n")
        
    # Label 2331 in original code
    # Conversion to end time
    for i in range(NC):
        XEOB[i] = X[i]*EXPDTO[i]
        SXEOB[i] = BINVRS[i,i]
        if (i+1) <= NV :
            L = i + NC
            SXEOB[i] = SXEOB[i] + (2*T0*BINVRS[i,L]) + ((T0**2)*BINVRS[L,L])
        if SXEOB[i] >= 0:
            SXEOB[i] = math.sqrt(SXEOB[i])*EXPDTO[i]
        elif SXEOB[i] < 0:
            print("Taking SQRT of ", SXEOB[i], " , NEGATIVE NUMBER!  Taking absolute value.  Do not trust results.")
            f.write("Tried to take square root of {0:12.4f}.  Since it was negative, absolute value was taken. \n Therefore, DO NOT TRUST THESE RESULTS!\n".format(SXEOB[i]))
            SXEOB[i] = math.sqrt(abs(SXEOB[i]))*EXPDTO[i]
        
    f.write("                 HALF LIFE         SIGMA H         CPM AT EOB     SIGMA EOB CPM    DECAY FACTOR \n")
    for i in range(NC):
        if NV < 0:
            time_char = "M"
        elif NV >= 0:
            time_char = H[i][-1:]
        #print(H_out[i], time_char, SIGMAH[i], time_char, XEOB[i], SXEOB[i], EXPDTO[i])
        f.write(" COMP({0:2d})\t{1:8.3f} {2:s}\t{3:8.3f} {4:s}\t{5:13.4E}\t{6:13.4E}\t{7:13.4E}\n".format(i+1, H_num[i], time_char, SIGMAH[i], time_char, XEOB[i], SXEOB[i], EXPDTO[i]))
        if HL[i] != 0:
            # Convert half-life back to user provided units
            if HL[i].endswith('S') or HL[i].endswith('s'):
                HL_num_out = ( ((math.log(2))*60) / DL[i] )
            elif HL[i].endswith('M') or HL[i].endswith('m'):
                HL_num_out = ( ((math.log(2))) / DL[i] )
            elif HL[i].endswith('H') or HL[i].endswith('h'):
                HL_num_out = ( ((math.log(2))/60) / DL[i] )
            elif HL[i].endswith('D') or HL[i].endswith('d'):
                HL_num_out = ( ((math.log(2))/60/24) / DL[i] )
            elif HL[i].endswith('Y') or HL[i].endswith('y'):
                HL_num_out = ( ((math.log(2))/60/24/365.25) / DL[i] )
            
            L_time_char = HL[i][-1:]
            f.write("          WITH {0:8.3f}{1:s} DAUGHTER. C1/C2 = {2:6.3f},  LBOM = {3:6.2f} minutes,  FACTOR = {4:6.3f} \n".format(HL_num_out,L_time_char,CEF[i],TBOM,FACT1[i]))
            
    if KCS != 0:
        i = NC
        j = 0
        while KCS > j:
            i = i + 1
            j = j + 1
            time_char2 = H[i-1][-1:]
            f.write(" COMP({0:2d})\t{1:8.3f} {2:s}\t\t\t\t{3:13.4E}\t{4:13.4E}                    KNOWN COMPONENT \n".format(i+1, H_num[i-1], time_char2, FKCS[j-1], SFKCS[j-1]))
    
    f.write("\n")
    f.write("\n")
    
    # Label 239 in original code
    VPV = 0.0
    for i in range(NP):
        FCALC[i] = 0
        for j in range(NC):
            FCALC[i] = FCALC[i] + (A[i,j]*X[j])
        V[i] = F[i] - FCALC[i]
        SF[i] = math.sqrt(SFSQ[i])
        RATIO_1[i] = V[i]/SF[i]
        VPV = VPV + ((V[i]**2)/SFSQ[i])
    
    # New to CLSQ2: R^2 calculation for fit.
    F_mean = np.mean(np.nonzero(F))
    SS_res = 0.0
    SS_tot = 0.0
    for i in range(NP):
        SS_tot = SS_tot + (F[i] - F_mean)**2
        SS_res = SS_res + (F[i] - FCALC[i])**2
    if SS_tot == 0:
        SS_tot = 1.0
    R_squared = 1 - (SS_res/SS_tot)
    
    DF = NP - NT
    if DF == 0:
        DF = 1.0
    sqrt_argument_2 = VPV/DF
    if sqrt_argument_2 >= 0 :
        FIT = math.sqrt(VPV/DF)
    elif sqrt_argument_2 < 0 :
        print("Taking SQRT of ", sqrt_argument_2, " , NEGATIVE NUMBER!  Taking absolute value.  Do not trust results.")
        f.write("Tried to take square root of {0:12.4f}.  Since it was negative, absolute value was taken. \n Therefore, DO NOT TRUST THESE RESULTS!\n".format(sqrt_argument_2))
        sqrt_argument_2 = abs(sqrt_argument_2)
        FIT = math.sqrt(sqrt_argument_2)
    
    f.write("    FIT = {0:7.3f}  ,  R-squared = {1:6.4f}\n".format(FIT,R_squared))
    f.write("\n")
    
    f.write("          T(I)                F(I)           FCALC(I)         V(I)          SIGMAF(I)       RATIO(I)  \n")
    for i in range(NP):
        f.write("    {0:13.4E}\t{1:13.4E}\t{2:13.4E}\t{3:13.4E}\t{4:13.4E}\t{5:9.2f}\n".format(time_adjusted[i], F[i], FCALC[i], V[i], SF[i], RATIO_1[i]))
    
    if (RJT != 0) and (NV == 0) :
        # Data rejection subroutine
        i = 0
        NRJT = 0
        
        loop2442 = 1 
        goto2443 = 0
        while loop2442 == 1:
            
            # "GO TO" processing zone
            # This section of the original code had many GO TO statements.
            # To preserve the logic, I've had to resort to using equivalent if statements and while loops.
            goto2449 = 0
            goto5010 = 0
            goto5001 = 0
            goto5021 = 0
            goto2457 = 0
            goto77 = 0
            goto1 = 0
            goto51 = 0
            #goto2442 = 0
            
            #print("i = ",i,", NP = ",NP)
            
            if goto2443 == 0:
                i = i + 1  # Label 2442 in original code
            # Note that index is starting at 1, so subtract 1 from index when referencing arrays
            goto2443 = 0 # Prevent goto2443 from getting stuck at 1
            
            if i <= NP:    # Label 2443 in original code
                while abs(RATIO_1[i-1]) <= abs(RJT):
                    i = i + 1
                    if i > NP:
                        goto2449 = 1
                        loop2442 = 0
                        if debug_mode == 1 : print("goto 2449")
                        break
            elif i > NP:
                goto2449 = 1
                loop2442 = 1
                if debug_mode == 1 : print("goto 2449 #2")
                break
            
            if goto2449 == 0:
                if RJT < 0:
                    goto5010 = 1
                    if debug_mode == 1 : print("goto 5010")
            if (goto2449 == 0) and (goto5010 == 0):
                if i == NP :
                    goto5001 = 1
                    if debug_mode == 1 : print("goto 5001")
            if (goto2449 == 0) and (goto5010 == 0) and (goto5001 == 0) :
                cond1 = RATIO_1[i-1]*RATIO_1[i]
                if cond1 <= 0:
                    goto5021 = 1
                    if debug_mode == 1 : print("goto 5021")
                elif cond1 > 0:
                    #goto2442 = 1
                    continue #return to label 2442
            if (goto5021 == 1) and (goto2449 == 0):
                if i == 1:  # Label 5021
                    goto5010 = 1
                    if debug_mode == 1 : print("goto 5010 #2")
            if (goto2449 == 0) and (goto5010 == 0): #(goto5001 == 1) and 
                cond2 = RATIO_1[i-1]*RATIO_1[i-2]
                if cond2 <= 0:
                    goto5010 = 1
                    if debug_mode == 1 : print("goto 5010 #3")
                elif cond2 > 0:
                    #goto2442 = 1
                    if debug_mode == 1 : print("goto 2442")
                    continue #return to label 2442
            
            if (goto5010 == 1) and (goto2449 == 0):
                NRJT = NRJT + 1
                BADT[NRJT-1] = time_adjusted[i-1]
                BADFS[NRJT-1] = F[i-1]
                NP = NP - 1
            
            # Move up remaining points
            if (goto2449 == 0):
                for j in range(i,NP+1):
                    time_adjusted[j-1] = time_adjusted[j]
                    F[j-1] = F[j]
                    SFSQ[j-1] = SFSQ[j]
                    RATIO_1[j-1] = RATIO_1[j]
                    for k in range(NT):
                        A[j-1,k] = A[j,k]
                
                goto2443 = 1
                if debug_mode == 1 : print("goto 2443")
                continue
            # This section above will loop until a GOTO statement sends it to label 2449
            
        # Label 2449 in original code
        if (goto2449 == 1):
            if NRJT == 0:
                f.write("\n All data points okay. None meet rejection criteria. \n")
                goto2457 = 1
            elif NRJT != 0:
                f.write("\n Data points rejected, {0:3d} are given below. \n     Bad T           Bad F \n".format(NRJT)) 
                for ii in range(NRJT):
                    f.write("{0:12.4E} \t {1:12.4E} \n".format(BADT[ii],BADFS[ii]))
                
                f.write("Repeat calculation without these points.\n")
                goto77 = 1
                if debug_mode == 1 : print("goto 77")
                continue
                    
    
    elif (RJT == 0) or (NV != 0) :
        goto2457 = 1
        if debug_mode == 1 : print("goto 2457")
    
    # Label 2547
    if LT1 == 1:
        LT1 = 0
        NV = NVTEMP
        goto51 = 1
        if debug_mode == 1 : print("goto 51")
        continue
    elif LT1 != 1:
        goto1 = 1
        break
        # I don't know why this was in the original code.  This basically restarts the code from the start where it reads in everything and defines variables.
        # For now, this "feature" will be ignored since reading input is handled differently here
        # Actually, I'm going to assume this just ends the code, serving as an exit condition.

if print_errors_to_file == 1:
    sys.stderr.close()
    sys.stderr = sys.__stderr__

# End of CLSQ
if debug_mode == 1 : print("DONE!")
# Write text to output file
#f = open(output_filename,"w+")
#f.write(output_text)
# Close opened file
f.close()

