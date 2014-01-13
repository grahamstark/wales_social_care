#
# Windows Main-Server farm install.
#
# uses Cygwin's cygrunsrv utility. Probably needs to be run with Admin privileges (CHECK THIS).
#
cygrunsrv.exe \
        --install mefisto_server \
        --path c:/VirtualWorlds/projects/mefisto/backend/ada/bin/mefisto_server.exe \
        --chdir c:/VirtualWorlds/projects/mefisto/backend/ada/ \
        --args "etc/be_settings_windows.txt etc/web_settings_windows.txt "
# start        
cygrunsrv.exe -S mefisto_server
 # query status
cygrunsrv.exe -Q mefisto_server -V 

