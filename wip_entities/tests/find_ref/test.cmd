!gnatmake -q main
PROJECT dummy.gpr

--  find all references to an entity
REFS proc1:main.adb:4:6

--  Params of Proc1
PARAMS proc1:main.adb:4:6

--  Same for another entity
REFS proc1:main.adb:5:6

