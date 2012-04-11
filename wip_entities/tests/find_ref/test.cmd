!gnatmake -q main
PROJECT dummy.gpr

--  find all references to an entity
REFS proc1:main.adb:4:6
PARAMS proc1:main.adb:4:6

--  Same, but print list of subprograms that call entity
CALLERS proc1:main.adb:4:6

--  Same for another entity
CALLERS proc1:main.adb:5:6

