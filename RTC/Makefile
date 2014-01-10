# MPLAB IDE generated this makefile for use with GNU make.
# Project: RTC.mcp
# Date: Sat Apr 06 14:23:33 2013

AS = mpasmwin.exe
CC = mcc18.exe
LD = mplink.exe
AR = mplib.exe
RM = rm

RTC.cof : i2c_common.o lcd.o main.o
	$(LD) /k"C:\Program Files\Microchip\MPASM Suite\LKR" "..\..\..\..\Program Files (x86)\Microchip\MPASM Suite\LKR\16f877_g.lkr" "i2c_common.o" "lcd.o" "main.o" /u_DEBUG /z__MPLAB_BUILD=1 /z__MPLAB_DEBUG=1 /o"RTC.cof" /M"RTC.map" /W

i2c_common.o : i2c_common.asm ../../../../Program\ Files\ (x86)/Microchip/MPASM\ Suite/p16f877.inc
	"C:\Program Files (x86)\Microchip\MPASM Suite\MPASMWIN.exe" /q /p16F877 "i2c_common.asm" /l"i2c_common.lst" /e"i2c_common.err" /o"i2c_common.o" /d__DEBUG=1

lcd.o : lcd.asm ../../../../Program\ Files\ (x86)/Microchip/MPASM\ Suite/p16f877.inc
	"C:\Program Files (x86)\Microchip\MPASM Suite\MPASMWIN.exe" /q /p16F877 "lcd.asm" /l"lcd.lst" /e"lcd.err" /o"lcd.o" /d__DEBUG=1

main.o : main.asm ../../../../Program\ Files\ (x86)/Microchip/MPASM\ Suite/p16f877.inc lcd.inc rtc_macros.inc
	"C:\Program Files (x86)\Microchip\MPASM Suite\MPASMWIN.exe" /q /p16F877 "main.asm" /l"main.lst" /e"main.err" /o"main.o" /d__DEBUG=1

clean : 
	$(RM) "i2c_common.o" "i2c_common.ERR" "i2c_common.lst" "lcd.o" "lcd.ERR" "lcd.lst" "main.o" "main.ERR" "main.lst" "RTC.cof" "RTC.hex"

