# Tableau and R Integration


Please go thru the PowerPoint file first. If you don't know what your are doing now.  


1. myLoop.R  
is a user-defined funcations in R. Move it into your R library directory.  
Find your R library directory. You may have multiple. I think any one should work.  
  .libPaths()


2. Tableau and R Integration.twbx  
Make sure R and Rserve are installed,  
and Rserver() is running,  
also set up the Tableau and R connection before you open the sample Tableau file.

3. sample_data.xlsx  
The Sample (fake) data has already extracted into Tableau. You don't need it at all.  
It is just for your reference.

4. Copy and paste Rprofile.site file to replace your Rprofile.site if it has nothing important there.  
Or, you can manually edit your Rprofile.site file to include the Rserve library auto-loading command.
[Your R install folder]\etc\Rprofile.site  
All the lines starting with # are comments in Rprofile.site, you can ignore them.
