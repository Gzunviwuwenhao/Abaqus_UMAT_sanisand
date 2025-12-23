# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2025 replay file
# Internal Version: 2024_09_20-21.00.46 RELr427 198590
# Run by wwh1998 on Wed Dec 24 01:12:02 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.776042, 0.775463), 
    width=114.233, height=76.9259)
session.viewports['Viewport: 1'].makeCurrent()
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
execfile('compression_model.py', __main__.__dict__)
#: model folder already exists
#: Folder already exists
#: The model database has been saved to "F:\mygithub\Abaqus_UMAT_sanisand\model\drained_P100_b0_Dr60.cae".
#: run_command  0.0525581836700439  seconds
print('RT script done')
#: RT script done
