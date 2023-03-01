# Governor's School of Computational-Physics

This is my portfolio of all of the scientific fortran codes I created during my time at the Governor's School of Computational Physics at Austin Peay State University. I utilized the Ubuntu Linux OS for compiling FORTRAN 95 and plotted the data points in gnuplot.
<h1><b>How to run the code</b><h1>
<h2>Linux</h2>
Simply input this command in order to install gfortran, the compiler for fortran 95.

> sudo apt-get install gfortran

Afterward, install gnuplot, as it is needed to plot the resulting data documents from the fortran code.
> sudo apt-get install gnuplot

Once you have both of these, place the fortran file you wish to run into a directory and go to that directory through the terminal. After that, input the command below in order to compile the fortran code:
> gfortran [Insert file name here, disreguard the brackets].f95

After that, check your directory. You will see that a new a.out file has been created. This is the output file that will actually run the fortran code.
> ./a.out

The code will run, and you will see some documents or text files created in the directory. Congratulations! The code has ran. 

Now, simply go to gnuplot:
> gnuplot
Then plot your documents.
> plot 'insert name here, make sure to keep quotations.fileextension' 
  
  # How to Plot 3-Dimensional Data Points
 
  In gnuplot, type in:
  > splot 'insert name here, make sure to keep quotations.fileextension'
