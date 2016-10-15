#SingleInstance force
#NoEnv 
#Persistent

SetBatchLines, -1
SetControlDelay, -1
SetWinDelay, -1
ListLines, Off
SetWorkingDir, %A_ScriptDir%
DetectHiddenWindows, Off
OnExit, DEATH
SetCapsLockState , AlwaysOff

global glo := {}

Gosub, Texts

InitShit()
glo.guiId := TheGui()
return

+F12::
^F12::
Reload:
Reload
Return

CapsLock::GuiToggleActive()
^CapsLock::GuiHide()

GuiToggleActive()
{
	if (WinActive("A")=glo.guiId)
		WinActivate,% "ahk_id " glo.la
	Else
	{
		glo.la := WinActive("A")
		WinShow,% "ahk_id " glo.guiId
		WinActivate,% "ahk_id " glo.guiId
		Send, !0
	}


}

GuiHide()
{
	WinHide,% "ahk_id " glo.guiId
}

CapsLock & `::
CapsLock & 1::
CapsLock & 2::
CapsLock & 3::
CapsLock & 4::
CapsLock & 5::
CapsLock & 6::
CapsLock & 7::
CapsLock & 8::
CapsLock & 9::CapsNum()

#If WinActive("ahk_id " glo.guiId)

^1::
^2::
^3::
^4::
^5::
^6::
^7::
^8::
^9::GoTab(SubStr(A_ThisHotkey, 2))

^T::NewTab()
^W::CloseTab(glo.ct)
^S::SaveTemplate()

#If

CapsNum()
{
	hk := A_ThisHotkey
	n	:= SubStr(hk, 12)
	n := (n="``") ? "Template" : n
	
		if !GetKeyState("Shift", "P")
			SendInput,% GetBox(n)
		Else
		{
			ocb := Clipboard
			Send, {Ctrl Down}c{Ctrl Up}
			ClipWait, 1		
			SetBox(n, Clipboard)
			Clipboard := ocb
		}
}

#If FocusEdit()
Up::
Down::FocusNav(A_ThisHotkey)
#If

FocusNav(dir)
{
GuiControlGet, hiw, CH:HWND, % "t" glo.ct "ibTempLst"
ControlSend,, {%dir%}, ahk_id %hiw%
}


FocusEdit() {
	ControlGetFocus, focCtrl,% "ahk_id " glo.guiId
	
	GuiControlGet, nejma, CH:name, %focCtrl%

	r := SubStr(nejma, -2)="CMD" ? 1 : 0
	return, %r%
}

TheGui() {
	global

	Gui, CH:Default
	Gui, Destroy
	Gui, +LastFound +ToolWindow +AlwaysOnTop -Caption -Border +Resize
	Gui, Margin, 5

	Gui, Menu, MyMenuBar

	Gui, Add, Tab3,y0 x0 gTabClick HWNDtabID vTabbe,% "1|+"
	SendMessage, TCM_SETMINTABWIDTH := 0x1331, 0,10,,ahk_id %tabID%
	Loop,% 9
	{
				AppendLayout("default",A_Index)
	} 
	glo.ct := 1
	glo.vt := {}
	glo.vt[1] := SaveTab(1)
	Gui, Show,,ClipHoarder
	WinMove,% "ahk_id " r := WinExist(), ,% A_ScreenWidth - glo.GuiWidth, 0, glo.GuiWidth+5, glo.GuiHeight+5

	Return r
}

AppendLayout(l="default",t=0)
{
	global

	local d, pyay, pya

	Gui, CH:Default
	Gui, Tab, %t%
	Loop,% glo.layouts[l].MaxIndex()
	{
		Gui, Add, Text,% "y+7 x5 w10 Right",% (A_Index > 9 ? "" : "&") . A_Index
		Gui, Add, Edit,% (A_Index=1?"x+5 Section ":"xs ") "yp-3 gBoxUpdate w243 r1 vt" t "ib"  A_Index,% glo.layouts[l][A_Index]
	}
	GuiControlGet, pya, Pos,% "t" t "ib"  glo.layouts[l].MaxIndex()
	Loop, 30
	{	
		d := A_Index + glo.layouts[l].MaxIndex()
		Gui, Add, Text,% "y+7 x5 w10 Right Hidden vt" t "lbl"  d,% (d > 9 ? "" : "&") . d
		Gui, Add, Edit,% "xs yp-3 gBoxUpdate w243 Disabled Hidden r1 vt" t "ib"  d, 1-UP!
	}

	Gui, Add, Text,% "x5 y" pyay+28, 12
	Gui, Add, Edit,% "xs yp-3 w100", name 
	Gui, Add, Edit,% "x+5 yp w100", last name
	Gui, Add, Edit,% "xs y+3 w200", street
	Gui, Add, Edit,% "x+5 yp w30", nr
	Gui, Add, Edit,% "xs y+5 w70", Zip
	Gui, Add, Edit,% "x+5 yp w120", City
	Gui, Add, Edit,% "x+5 yp w30", Re

	Gui, Add, Text,% "w" glo.guiWidth " x0 y+10"  " gPungt h10 Center vt" t "pungt", ~~~~~~~~~~~~~~
	GuiControlGet, pya, Pos,% "t" t "pungt"
	glo.layouts[l].pungspaerrTop := pyay
	Gui, Add, Text,% "x5 y+2 w10 Right vt" t "nollan", &0
	Gui, Add, Edit,% "gBoxUpdate Section w" 260-113 " yp-3 xs r1 vt" t "ibCMD",
	Gui, Add, Edit,% "gBoxUpdate h78 x8 y+5 w" 260-102 " vt" t "ibTemplate", 
	Gui, Add, ComboBox,% "ys x" 8+163 " w" 260-168 " gCbAutoComplete vt" t "ibTempSel",% glo.layouts[l]["templates"]["list"] "||" 
	Gui, Add, ListBox,% "x" 8+163 " y+5 sort w" 260-168 " vt" t "ibTempLst gBoxUpdate hwndhlblList -HScroll +256",% FilterList(glo.layouts[l]["templates"][GetBox("TempSel",t)],GetBox("CMD",t))
	PungtMove(pyay+5, t)
}

Pungt()
{
	CoordMode, Mouse, Window
	While, GetKeyState("LButton", "P")
	{
		MouseGetPos,, my
		PungtMove(my)
		
		
	}
	;GuiControl, CH:MoveDraw,% "t" glo.ct "ibCMD" , y%my% 
}

PungtMove(my,t="ct")
{
	static oy

	if (my=oy)
		Return
	oy := my
	t := t="ct"?glo.ct:t
	pst := glo.layouts["default"].pungspaerrTop
	glo.vt[t].psp := my
	my := (my < pst) ? pst-22 : (my>(glo.guiHeight-30) ? glo.guiHeight-30 : my)-30
	Hiddrik((my+30) - pst, t) 
	SetBox(11,glo.guiHeight - my)

	GuiControl, CH:MoveDraw,% "t" t "pungt" , y%my% 
	GuiControl, CH:MoveDraw,% "t" t "ibCMD" ,% "y" my+9
	GuiControl, CH:MoveDraw,% "t" t "ibTempSel" ,% "y" my+9
	GuiControl, CH:MoveDraw,% "t" t "ibTemplate" ,% "y" my+35 " h" (glo.guiHeight - (my+100))
	GuiControl, CH:MoveDraw,% "t" t "ibTempLst" ,% "y" my+35 " h" (glo.guiHeight - (my+100))
	GuiControl, CH:MoveDraw,% "t" t "nollan" ,% "y" my+12

}

Hiddrik(dif,t)
{
	vis := Floor(dif/28)
	Loop, 30
	{
		op := A_Index > vis ? "+" : "-" 
		GuiControl, CH:%op%Hidden %op%Disabled,% "t" t "ib" A_Index + glo.layouts["default"].MaxIndex()
		GuiControl, CH:%op%Hidden ,% "t" t "lbl" A_Index + glo.layouts["default"].MaxIndex()
	}
}

BoxUpdate()
{
	s := StrSplit(SubStr(A_GuiControl, 2), "ib"), t := s[1], cntrl := s[2]

	bc := glo["vt"][t]["ib" cntrl] := GetBox(cntrl,t)

	if (cntrl="CMD")
	{
		SetBox("TempLst", "|" ,t)
		SetBox("TempLst", FilterList(glo.layouts["default"]["templates"][GetBox("TempSel", t)],bc) ,t)	
		FocusNav("Down")
	}

	else if (cntrl="TempLst")
	{
		SetBox("Template", ReadTemplate(bc,GetBox("TempSel",t),"default"),t)
		glo["vt"][t]["templateSave"] := Templation(GetBox("Template",t),"w")
	}
	else if (cntrl="Template")
	{
		glo["vt"][t]["templateSave"] := Templation(bc,"w")
	}
	else
		SetBox("Template",Templation(glo["vt"][t]["templateSave"],"r"))
}

SetBox(b,v="",t="ct")
{
	t := t="ct" ? glo.ct : t
	GuiControl, CH:,% "t" t "ib" b , %v%

}

GetBox(b,t="ct")
{
	t := t="ct" ? glo.ct : t
	GuiControlGet, v, CH:,% "t" t "ib" b

	return v
}

CbAutoComplete()
{	; CB_GETEDITSEL = 0x0140, CB_SETEDITSEL = 0x0142
	If ((GetKeyState("Delete", "P")) || (GetKeyState("Backspace", "P")))
		return
	GuiControlGet, lHwnd, Hwnd, %A_GuiControl%
	SendMessage, 0x0140, 0, 0,, ahk_id %lHwnd%
	MakeShort(ErrorLevel, Start, End)
	GuiControlGet, CurContent,, %lHwnd%
	GuiControl, ChooseString, %A_GuiControl%, %CurContent%
	If (ErrorLevel)
	{
		ControlSetText,, %CurContent%, ahk_id %lHwnd%
		PostMessage, 0x0142, 0, MakeLong(Start, End),, ahk_id %lHwnd%
		return
	}
	GuiControlGet, CurContent,, %lHwnd%
	PostMessage, 0x0142, 0, pen := MakeLong(Start, StrLen(CurContent)),, ahk_id %lHwnd%
	s := StrSplit(SubStr(A_GuiControl, 2), "ib")
	glo["vt"][s[1]]["ib" s[2]] := CurContent
	SetBox("TempLst", "|" ,s[1])
	SetBox("TempLst", FilterList(glo.layouts["default"]["templates"][CurContent],GetBox("CMD",s[1])) ,s[1])
	FocusNav("Down")
}

FilterList(l,f)
{
	if !(f="")
	{
		al := StrSplit(l, "|")
		Loop,% al.MaxIndex()
	 		nl .= (al[A_Index] ~= f) ? al[A_Index] "|" : ""
		StringTrimRight, l, nl, 1
	}
	Return l
}

MakeLong(LoWord, HiWord)
{
	return (HiWord << 16) | (LoWord & 0xffff)
}

MakeShort(Long, ByRef LoWord, ByRef HiWord)
{
	LoWord := Long & 0xffff
,   HiWord := Long >> 16
}

SaveTab(t)
{
	o := {}
	For k, v in glo.layouts["Default"]
	{
		if (k = "templates")
			Continue

		o["ib" k] := GetBox(k, t)
	}

	o["ibTemplate"] := GetBox("Template", t)

	Return o
}

LoadTab(t)
{
	For k, v in glo.vt[t]
	{
		if (k = "layout")
			Continue
		else	
			GuiControl, CH:, t%t%%k%, %v%
		 	PungtMove(glo.vt[t].psp, t)
	}
}

TabClick()
{
	GuiControlGet, dd,CH:, Tabbe

	If (dd = "+")
		NewTab()
	Else
		glo.ct := dd
}

NewTab()
{
	glo.vt.Push(SaveTab(glo.vt.MaxIndex()+1))
	GoTab(glo.ct := glo.vt.MaxIndex())
}

GoTab(t)
{
	GuiControl, CH:, Tabbe, |
	Loop,% glo.vt.MaxIndex()
		lst .= (((A_Index = 1) ? "" : "|") A_Index ((A_Index = t) ? "|" : ""))
	lst .= "|+"

	GuiControl, CH:, Tabbe, %lst%
}

CloseTab(t)
{
	glo.vt.RemoveAt(t)
	Loop,% glo.vt.MaxIndex()
		LoadTab(A_Index)
	GoTab(glo.ct := glo.vt.MaxIndex())
}

ReadLayouts()
{
	glo.layouts := {}


	Loop,% A_ScriptDir "\Layouts\*.*",2
	{		
		tt := A_LoopFileName
		glo.layouts[tt] := {}
		Loop, Read,% A_ScriptDir "\Layouts\" tt "\list"
			glo.layouts[tt][A_Index] := A_LoopReadLine
		glo.layouts[tt]["templates"] := {}
		Loop,% A_ScriptDir "\Layouts\" tt "\*.*",2
		{
			glo.layouts[tt]["templates"][tn := A_LoopFileName] := ""
			glo.layouts[tt]["templates"]["list"] .= ( A_Index=1 ? "" : "|" ) A_LoopFileName
			Loop,% A_LoopFileFullPath "\*.*"
				glo.layouts[tt]["templates"][tn] .= (A_Index = 1 ? "" : "|") A_LoopFileName
		}
			
	}
}

ReadTemplate(t,d,l)
{
	if !f := FileOpen(A_ScriptDir "\Layouts\" l "\" d "\" t, 0)
		Return 0

	f.Seek(0)
	r := Templation(f.Read(),"r")
	f.Close()
	Return,% r
}

SaveTemplate()
{
	static stname, stfolder

	Gui, ST:Default
	Gui, Destroy
	Gui, +ToolWindow
	GuiControl, CH:+AltSubmit, % "t" glo.ct "ibTempSel"
	GuiControlGet, lll, CH:,% "t" glo.ct "ibTempSel"
	GuiControl, CH:-AltSubmit, % "t" glo.ct "ibTempSel"
	Gui, Add, Edit,w175 vstname,% GetBox("TempLst")
	Gui, Add, ComboBox,% "w111 yp x+5 vstfolder +Choose" lll ,% glo.layouts["default"]["templates"]["list"] "" 
	Gui, Add, Button, w80 xm y+5 +Default, Save
	Gui, Show,, Save Template as:

	Return

	STButtonSave:
	Gui, ST:Submit
	d := A_ScriptDir "\layouts\default\" stfolder
	f := FileOpen(d "\" stname, "w")
	f.Seek(0)
	f.Write(glo.vt[glo.ct].templateSave)
	f.Close()

	Loop, %d%\*.*
		s .= A_LoopFileName "|"
	glo.layouts["default"]["templates"][stfolder] := s
	SetBox("TempLst","|")
	SetBox("TempLst",s)
	Return

	STGuiEscape:
	STGuiClose:
	Gui, ST:Submit
	Return

}

UpdateTemps()
{
	cbLst := ""

	Loop,% A_ScriptDir "\Layouts\" glo["tabs"][glo.ct].layout "\templates\*.*"
		cbLst .= (A_Index=1?"":"|") A_LoopFileName 
	glo["tabs"][glo.ct].lstTemps := cbLst	
}

Templation(ttt,m="t")
{
	t := glo.ct
	l := "default"

	Loop ,% glo.layouts["default"].MaxIndex()
	{
		balko := "`%" glo.layouts["default"][A_Index] "`%"
		lkewk := GetBox(A_Index, t)

		if (m="w")
			StringReplace, ttt, ttt,%lkewk%,%balko%, All
		if (m="r")
			StringReplace, ttt, ttt,%balko%,%lkewk%, All

	}
	Return ttt
	;Loop, Parse, InputVar [, Delimiters|CSV, OmitChars]
}

TrayMenu() {
	Menu, Tray, NoStandard
	Menu, Tray, Add, Exit, DEATH
	Menu, Tray, Add, Show/Hide, ToggleGui
	Menu, Tray, Default, Show/Hide
	; could be checkmarked 'visible'
	Menu, Tray, Add, Reload

}

MenuBar()
{
	Menu, TemplateMenu, Add, &Save`tCtrl+S, SaveTemplate  ; See remarks below about Ctrl+O.
	Menu, TemplateMenu, Add, &Delete, BoxUpdate
	Menu, TabMenu, Add, &New`tCtrl+T, NewTab
	Menu, TabMenu, Add, &Close`tCtrl+W, CloseTab
	Menu, HelpMenu, Add, &About, BoxUpdate
	Menu, MyMenuBar, Add, &Tab, :TabMenu
	Menu, MyMenuBar, Add, &Template, :TemplateMenu  ; Attach the two sub-menus that were created above.
	Menu, MyMenuBar, Add, &Help, :HelpMenu
	Menu, MyMenuBar, Add, E&xit, DEATH
}

ToggleGui() 
{

	if ((glo.guiId = WinActive("ahk_id " glo.guiId)) && glo.guiVisible)
	{
		WinHide,% "ahk_id " glo.guiId
		glo.guiVisible := 0
		Return
	}

	WinShow,% "ahk_id " glo.guiId		
	WinActivate,% "ahk_id " glo.guiId
	Send, !0
	glo.guiVisible := 1

}

CreateDefaultLayout()
{
	FileCreateDir,% d:= A_ScriptDir "\Layouts\default"
	FileCreateDir,% dt:= A_ScriptDir "\Layouts\default\templates"
	dlo := "CUSTOMER ID,S/N,SR,MAIL,DESCRIPTION,FIRST NAME,LAST NAME,STREET,POSTALCODE,CITY,SO"
	Loop, Parse, dlo , `,
		s .= (A_Index=1 ? "" : "`n") . A_LoopField  

	FileAppend, %s%, %d%\list
	FileAppend,% glo.txtHlp, %dt%\index
} 

CreateList(filter = "") { 
	Static sLastfilter := "`n"  ;Initialize on an impossible filter

	;Trim the right side
	While (SubStr(filter, 0) = A_Space)
		StringTrimRight, filter, filter, 1

	;Trim right side if it ends in " !" since it changes nothing
	If (StrLen(filter) > 2) And (SubStr(filter, -1) = " !") And (SubStr(filter, -2, 1) <> A_Space)
		StringTrimRight, filter, filter, 2

	;Check if the filter is different
	If (filter = sLastfilter) ;And bShowing
		Return

	sLastfilter := filter
	t := glo.ct
	StringLen, tl, t
	visi := "t"  (tl < 2 ? "0" t : t) 
	GuiControl, CH:-Redraw,% visi "CCC"
	GuiControl,CH:,% visi "CCC",|
	lblList := ""

	If (filter = "") 
		lblList := glo["tabs"][glo.ct].lstTemps

	 Else {
	 	listig := glo["tabs"][glo.ct].lstTemps
		
		Loop, Parse, listig, |
		{
			If (InStr(A_LoopField, filter)) {
				lblList .= "|" A_LoopField
			}
		}
	}
	GuiControl,CH:,% visi "CCC",% !lblList ? "|" : lblList

	GuiControlGet, hiw, CH:HWND, % visi "CCC"
	;SendMessage (LB_GETCURSEL := 0x0188), 0, 0,, ahk_id %hiw%

	;Redraw
	GuiControl, CH:+Redraw,% visi "CCC"
	FocusNav("Down")
}

ToggleMenu( hWin , m="t")
{
	static hMenu, visible
	if hMenu =
		hMenu := DllCall("GetMenu", "uint", hWin)
	
	if m="s"
	{
		DllCall("SetMenu", "uint", hWin, "uint", hMenu)
		visible := True
		Return
	}

	if m="h"
	{
		DllCall("SetMenu", "uint", hWin, "uint", 0)
		visible := False
		Return
	}

	if !visible
			DllCall("SetMenu", "uint", hWin, "uint", hMenu)
	else	DllCall("SetMenu", "uint", hWin, "uint", 0)

	visible := !visible
}

InitShit() {

	If (kuk:=FileExist(A_ScriptDir "\Layouts"))!="AD"
		CreateDefaultLayout()

	ReadLayouts()
	TrayMenu()
	MenuBar()

	SysGet, mwa, MonitorWorkArea
	glo.guiHeight := mwaBottom
	glo.guiWidth := 275
}

Texts:
Help = 
(
This is Clipping Tool

A clipboard and template manager for fixed systems.

Toggle Window: 
CapsLock

Insert text from a field: 
Ctrl+F1-F12

Insert selected text to a field: 
Alt+F1-F12

Navigate in window with: 
Alt+0-9

Save template: 
Ctrl+S

Insert template: 
Ctrl+Capslock
)

glo.txtHlp := Help
return

DEATH:
ExitApp