copy WinSW.NET4.exe s-luna-empire.exe
copy WinSW.NET4.exe s-luna-undo-redo.exe
copy WinSW.NET4.exe s-luna-ws-connector.exe
copy WinSW.NET4.exe s-luna-broker.exe
s-luna-empire install
s-luna-undo-redo install
s-luna-ws-connector install
s-luna-broker install

:: below is a super cryptic way to allow local users to start/stop/restart/query status of Luna services
:: how does it work?
:: 1. first of all, default security descriptor for our installed service is
::    "D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWLOCRRC;;;SU)S:(AU;FA;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;WD)"
::    as can be obtained using "sc.exe sdshow _service_"
:: 2. explanation of the above is available at https://support.microsoft.com/en-us/help/914392/best-practices-and-guidance-for-writers-of-service-discretionary-acces
:: 3. to allow local users to manipulate our services, we add a new section containing
::    RP (start), WP (stop), DT (pause/continue), LO (query status) permissions for BU (Built-in (Local) Users)
::    our new section looks like "(A;;RPWPDTLO;;;BU)"
:: 4. This needs to be added to _discretionary ACL_ (DACL) of security descriptor (part with D: at the beginning)
:: 5. Finally, we set the new descriptor on all of our services using "sc.exe sdset _service_ _descriptor_"

%SystemRoot%\System32\sc.exe sdset s-luna-empire "D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWLOCRRC;;;SU)(A;;RPWPDTLO;;;BU)S:(AU;FA;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;WD)"
%SystemRoot%\System32\sc.exe sdset s-luna-undo-redo "D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWLOCRRC;;;SU)(A;;RPWPDTLO;;;BU)S:(AU;FA;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;WD)"
%SystemRoot%\System32\sc.exe sdset s-luna-ws-connector "D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWLOCRRC;;;SU)(A;;RPWPDTLO;;;BU)S:(AU;FA;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;WD)"
%SystemRoot%\System32\sc.exe sdset s-luna-broker "D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWLOCRRC;;;SU)(A;;RPWPDTLO;;;BU)S:(AU;FA;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;WD)"
