# CLSQ2

### Cumming’s Least Squares (version 2)
This is a modernized version of the 1962 Cumming’s Least Squares (CLSQ) Brookhaven Decay Curve Analysis Program, written in FORTRAN IV.  The original paper on it can be read [<u>here</u>](https://books.google.com/books?id=DZQrAAAAYAAJ&lpg=PR1&pg=PA25#v=onepage&q&f=false). CLSQ analyzes multicomponent decay curves by a least-squares procedure to iteratively identify the half-lives and abundances of the individual isotopes of a measured sample.  Its use at Oak Ridge National Laboratory (ORNL) had been limited to a few very old computers due to its age and recompilation issues; only a 16-bit compiled version of the code was in use at ORNL.  To make this code more accessible, it has been translated the original code from FORTRAN IV into Python 3.  Additionally, the input and output functionalities of CLSQ have been improved, making it more user-friendly.  

### Instructions

See the CLSQ2 Documentation file within the Documentation folder for detailed coverage of CLSQ2.

CLSQ2 should be distributed as a single ZIP file.  Extract the files to a single folder.  When unpacked, the file structure should appear as shown below. 

    CLSQ2 
        README.txt
        Documentation
            CLSQ2 Documentation 
            J. B. Cumming Paper
        Executable_CLSQ2
            CLSQ2.exe
            *.dll (4 files)
            tcl (folder)
        Source_Code_CLSQ2
            CLSQ2.py	
            setup.py
        Examples
        Original_CLSQ
            CLSQ.exe
            CLSQ.FOR
            Input_guide.pdf
            Examples (folder)

It is advised that all of these files be kept together.  The only true requirement though is that all of the “.dll” files and the “tcl” folder within the “Executable_CLSQ2” folder must stay together with the “.exe” file for the executable to work.  Feel free to make a shortcut to the executable and place it somewhere more convenient.  

After creating an input file (see next section for detailed input guide), the code is run simply by running the CLSQ2.exe file (double-click, right-click & Run, using the command window, etc.).  When the program is run, a file dialogue window should pop up; select your input file from this window.  By default the folder window will open in the same directory as the executable, so it is advised that input files be placed in a nearby directory for the sake of convenience.  The program will then run, and the output file should appear in the same directory as the input file with the same filename except with a “.out” extension.  This output file is just a text file, so it can be opened in any text editor.  See the output section for a detailed overview of the meaning of items in the output file.

If you have a version of Python 3 installed along with the NumPy library, you may just run the Python script CLSQ2.py located in the Source_Code_CLSQ2 folder if desired.

The original CLSQ code is included just for bookkeeping purposes.  Feel free to look through the old input files and source code, but note that the CLSQ.exe file will not run on 64-bit computers.  
