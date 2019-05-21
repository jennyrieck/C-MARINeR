Wrap it up into a package

Modules to do:

1. input
   * takes in data and returns array (3D matrix)
   * optional additional array - format individual datafiles
   * optional input is row/column design to color factormap plot
   * Module retuns: array and optional design (may or maynot have ui component depending on if we read data in with the function)
       *ui is a maybe for now so empty module

2. analysis
    * input: dataarray, ui specified: tablenorm type (as a radio button option, 3 options = "SS1", "MFA", or "none"
    * trigger button to run analysis 
    * return outputs from covstatis() - compact code; list of 
    
3. Screeplot module RV and compromise
    * no user specs

4. plotting options for factor map - ui specified:
   * Select components to plot (x axis, y axis)
   * trigger to plot
   
   * interactive features: zooming, hoverpoints,    
   
5. plotting options for heatmap - ui specified:
   * Select range of components to plot (slide bar that scales from 1 to total # of components)
