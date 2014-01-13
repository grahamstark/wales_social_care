#
# Windows Main-Server farm install.
#
# uses Cygwin's cygrunsrv utility. Probably needs to be run with Admin privileges (CHECK THIS).
#
cygrunsrv.exe \
        --install wsc_server \
        --path C:/VirtualWorlds/projects/wales_social_care/model2/bin/wsc_server.exe \
        --chdir C:/VirtualWorlds/projects/wales_social_care/model2/ \
        --args "etc/global_settings_windows.txt "
# start        
cygrunsrv.exe -S wsc_server
 # query status
cygrunsrv.exe -Q wsc_server -V 

