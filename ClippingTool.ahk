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

global glo := {}, nl := "`n"

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
CapsLock & x::FlushTab()
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
^9::GoTab(SubStr(A_ThisHotkey, 2),glo.cl)

!1::
!2::
!3::
!4::
!5::
!6::
!7::
!8::
!9::GoBox(SubStr(A_ThisHotkey, 2))

!0::
GuiControl, CH:Focus,% "t" glo.cl glo.ct "ibCMD"
SendInput, ^a
return

^T::NewTab()
^W::CloseTab(glo.ct)
^S::SaveTemplate()

#If

GoBox(n)
{
	box := glo.layouts[glo.cl]["layout"][n][1][1]
	varis := "t" glo.cl glo.ct "ib" n box.name
	GuiControl, CH:Focus,% varis
	if ErrorLevel
		GuiControl, CH:Focus,% "t" glo.cl glo.ct "ib" n
	SendInput, ^a
}

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
	GuiControlGet, hiw, CH:HWND, % "t" glo.cl glo.ct "ibTempLst"
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
	Gui, Margin, 3

	Gui, Menu, MyMenuBar

	l := glo.cl := "default"
	t := glo.ct := 1

	For, k in glo.layouts
	{
		Gui, Add, Tab3,y0 x0 gTabClick HWNDtabID%k% vTabbe%k%,% "1|+"
		SendMessage, TCM_SETMINTABWIDTH := 0x1331, 0,10,,% "ahk_id " tabID%k%
		Loop,% 9
			AppendLayout(k,A_Index)
		GuiControl, CH:MoveDraw, Tabbe%k%,y-1000
		glo.layouts[k].vt := {}
		glo.layouts[k].vt[t] := SaveTab(t,l)
	}

	GuiControl, CH:MoveDraw, Tabbe%l%, y0
	Gui, Show,,ClipHoarder
	WinMove,% "ahk_id " r := WinExist(), ,% A_ScreenWidth - glo.winW, 0, glo.winW+5, glo.winH+5

	Return r
}

AppendLayout(l="default",t=0)
{
	global

	local d, pyay, pya

	Gui, CH:Default
	Gui, Tab, %t%
	;[siffer][grupp][nummer]
	og := 1
	Loop,% glo.layouts[l].layout.MaxIndex()
	{
		mi := A_Index
		Gui, Add, Text,% "y+7 xm w10 Right",% (A_Index > 9 ? "" : "&") . mi
		Loop, % ng := glo.layouts[l]["layout"][mi].MaxIndex()
		{
			gl := A_Index
			Loop, % cp := glo.layouts[l]["layout"][mi][gl].MaxIndex()
			{
				widar := (glo.winW-(16+(cp*3)))/100
				box := glo.layouts[l]["layout"][mi][gl][A_Index]
				varis := "t" l t "ib" mi (((cp=1) && (ng=1)) ? "" : box.name)
				opt := ""
					.	"x" ((mi=1) ? "+5 Section " : ((A_Index!=1) ? "+3 " : "s "))
					.	"y" ((A_Index!=1) ? ((gl!=og) ? "+3 " : "p ") : ((gl!=og) ? "+3 " : "p-3 "))
					.	"w" Floor(widar*box.W) " "
					.	"v" varis " gBoxUpdate r" box.H
				Gui, Add, Edit,%opt%,% box.name
				og := gl
			}		
		}
	}

	GuiControlGet, pya, Pos,% varis
	Loop, 30
	{	
		d := A_Index + mi
		;msgbox,% "vt" t "lbl" d
		Gui, Add, Text,% "y+7 xm w10 Hidden Right vt" l t "lbl" . d,% (d > 9 ? "" : "&") d
		Gui, Add, Edit,% "xs yp-3 Hidden gBoxUpdate w243 Disabled  r1 vt" (l t . "ib" . d), 1-UP!
	}
	
	Gui, Add, Text,% "w" glo.winW " x0 y" pyay+25 " gPungt h10 Center vt" l t "pungt", ~~~~~~~~~~~~~~
	GuiControlGet, pya, Pos,% "t" t "pungt"
	glo.layouts[l].pungspaerrTop := pyay+25
	Gui, Add, Text,% "xm y+2 w10 Right vt" l t "nollan", &0
	Gui, Add, Edit,% "gBoxUpdate Section w" 260-113 " yp-3 xs r1 vt" l t "ibCMD",
	Gui, Add, Edit,% "gBoxUpdate h78 x5 y+5 w" 260-102 " vt" l t "ibTemplate", 
	Gui, Add, ComboBox,% "ys x" 5+163 " w" 260-168 " gCbAutoComplete vt" l t "ibTempSel",% glo.layouts[l]["templates"]["list"] "||" 
	Gui, Add, ListBox,% "x" 5+163 " y+5 sort w" 260-168 " vt" l t "ibTempLst gBoxUpdate hwndhlblList -HScroll +256",% FilterList(glo.layouts[l]["templates"][GetBox("TempSel",t)],GetBox("CMD",t))
	PungtMove(pyay+5, t, l)
}

CHGuiSize:
if (A_GuiWidth != glo.winW)
	ReSajza(A_GuiWidth,glo.ct,glo.cl)
Return

ReSajza(gw,t,l)
{
	GuiControl, CH:MoveDraw, Tabbe%l%,% "w" gw+4

	Loop,% glo.layouts[l].layout.MaxIndex()
	{
		mi := A_Index
		Loop, % ng := glo.layouts[l]["layout"][mi].MaxIndex()
		{
			gl := A_Index
			Loop, % cp := glo.layouts[l]["layout"][mi][gl].MaxIndex()
			{
				widar := (gw-(16+(cp*3)))/100
				box := glo.layouts[l]["layout"][mi][gl][A_Index]
				varis := "t" l t "ib" mi (((cp=1) && (ng=1)) ? "" : box.name)
				GuiControl, CH:MoveDraw, % varis,% "w" Floor(widar*box.W)
				if A_Index != 1
					GuiControl, CH:MoveDraw, % varis,% "x" posax+posaw
				og := gl
				GuiControlGet, posa, CH:Pos, %varis%
			}		
		}
	}

	Loop, 35
		GuiControl, CH:MoveDraw, % "t" l t "ib" A_Index+mi,% "w" gw-21

	GuiControl, CH:MoveDraw, % "t" l t "ibCMD",% "w" gw-117
	GuiControl, CH:MoveDraw, % "t" l t "ibTemplate",% "w" gw-104
	GuiControl, CH:MoveDraw, % "t" l t "ibTempSel",% "x" gw-97
	GuiControl, CH:MoveDraw, % "t" l t "ibTempLst",% "x" gw-97

	glo.winW := gw
}

return
Pungt()
{
	CoordMode, Mouse, Window
	While, GetKeyState("LButton", "P")
	{
		MouseGetPos,, my
		PungtMove(my)
	}
}

PungtMove(my,t="ct",l="cl")
{
	static oy, ot, ol

	t := (t="ct")?glo.ct:t
	l := (l="cl")?glo.cl:l

	if (my=oy) && (t=ot) && (l=ol)
		Return
	oy := my
	ol := l
	ot := t
	

	pst := glo.layouts[l].pungspaerrTop
	glo.layouts[l].vt[t].psp := my
	my := (my < pst) ? pst-22 : (my>(glo.winH-30) ? glo.winH-30 : my)-30
	Hiddrik((my+30) - pst, t, l) 
	SetBox(11,glo.winH - my)

	GuiControl, CH:MoveDraw,% "t" l t "pungt" , y%my% 
	GuiControl, CH:MoveDraw,% "t" l t "ibCMD" ,% "y" my+9
	GuiControl, CH:MoveDraw,% "t" l t "ibTempSel" ,% "y" my+9
	GuiControl, CH:MoveDraw,% "t" l t "ibTemplate" ,% "y" my+35 " h" (glo.winH - (my+100))
	GuiControl, CH:MoveDraw,% "t" l t "ibTempLst" ,% "y" my+35 " h" (glo.winH - (my+100))
	GuiControl, CH:MoveDraw,% "t" l t "nollan" ,% "y" my+12

}

Hiddrik(dif,t,l)
{
	vis := Floor(dif/28)
	Loop, 30
	{
		op := A_Index > vis ? "+" : "-" 
		GuiControl, CH:%op%Hidden %op%Disabled,% "t" l t "ib" A_Index + glo.layouts[l].layout.MaxIndex()
		GuiControl, CH:%op%Hidden ,% "t" l t "lbl" A_Index + glo.layouts[l].layout.MaxIndex()
	}
}

BoxUpdate()
{
	; "t" l t "ib" A_Index
	s := StrSplit(SubStr(A_GuiControl, 2), "ib")
 ,l := SubStr(s[1],1,-1)
 ,t := SubStr(s[1],-0)
 ,cntrl := s[2]

	bc := glo.layouts[l]["vt"][t]["ib" cntrl] := GetBox(cntrl,t,l)

	if (cntrl="CMD")
	{
		SetBox("TempLst", "|" ,t,l)
		SetBox("TempLst", FilterList(glo.layouts[l]["templates"][GetBox("TempSel", t,l)],bc),t,l)	
		FocusNav("Down")
	}

	else if (cntrl="TempLst")
	{
		SetBox("Template", ReadTemplate(bc,GetBox("TempSel",t,l),l),t,l)
		glo.layouts[l]["vt"][t]["templateSave"] := Templation(GetBox("Template",t,l),"w")
	}
	else if (cntrl="Template")
	{
		glo.layouts[l]["vt"][t]["templateSave"] := Templation(bc,"w")
	}
	else
		SetBox("Template", Templation(glo.layouts[l]["vt"][t]["templateSave"],"r"),t,l)
}

SetBox(b,v="",t="ct",l="cl")
{
	t := t="ct" ? glo.ct : t
	l := l="cl" ? glo.cl : l
	GuiControl, CH:,% "t" l t "ib" b , %v%

}

GetBox(b,t="ct",l="cl")
{
	t := t="ct" ? glo.ct : t
	l := l="cl" ? glo.cl : l
	GuiControlGet, v, CH:,% "t" l t "ib" b

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
 ,l := SubStr(s[1],1,-1)
 ,t := SubStr(s[1],-1)
 ,cntrl := s[2]
	glo.layouts[l]["vt"][t]["ib" cntrl] := CurContent
	SetBox(b,v="",t="ct",l="cl")
	SetBox("TempLst", "|" ,t,l)
	SetBox("TempLst", FilterList(glo.layouts[l]["templates"][CurContent]
 ,GetBox("CMD",t,l)),t,l)
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

SaveTab(t, l)
{
	o := {}
	For k, v in glo.layouts[l]["vt"][t]
	{
		if (k = "templates")
			Continue

		o["ib" k] := GetBox(k, t, l)
	}

	o["ibTemplate"] := GetBox("Template", t, l)

	Return o
}

LoadTab(t,l)
{
	For k, v in glo.layouts[l].vt[t]
	{
		if (k = "layout")
			Continue
		else	
			GuiControl, CH:, t%t%%k%, %v%
		 	PungtMove(glo.layouts[l].vt[t].psp, t)
	}
}

TabClick()
{
	GuiControlGet, dd,CH:,% "Tabbe" glo.cl

	If (dd = "+")
		NewTab()
	Else
	{
		glo.ct := dd
		ReSajza(glo.winW,dd,glo.cl)
	}
}

FlushTab()
{
	Loop,% glo.layouts[glo.cl].layout.MaxIndex()
	{
		mi := A_Index
		Loop, % ng := glo.layouts[glo.cl]["layout"][mi].MaxIndex()
		{
			gl := A_Index
			Loop, % cp := glo.layouts[glo.cl]["layout"][mi][gl].MaxIndex()
			{
				box := glo.layouts[glo.cl]["layout"][mi][gl][A_Index]
				varis := "t" glo.cl glo.ct "ib" mi (((cp=1) && (ng=1)) ? "" : box.name)

				GuiControl, CH:,%  varis,% box.name
				og := gl
			}		
		}
	}

	Loop, 30
	{	
		d := A_Index + mi
		GuiControl, CH:,% "t" glo.cl glo.ct "ib" d,1-UP!
	}


}



NewTab()
{
	if (glo.layouts[glo.cl].vt.MaxIndex() = 9)
		return 
	glo.layouts[glo.cl].vt.Push(SaveTab(glo.layouts[glo.cl].vt.MaxIndex()+1,glo.cl))
	GoTab(glo.ct := glo.layouts[glo.cl].vt.MaxIndex(),glo.cl)
}

GoTab(t,l)
{	
	ReSajza(glo.winW,t,l)
	GuiControl, CH:, Tabbe%l%, |
	Loop,% glo.layouts[l].vt.MaxIndex()
		lst .= ((A_Index = 1) ? "" : "|") 
				. A_Index . ((A_Index = t) ? "|" : "")
 
	GuiControl, CH:,% "Tabbe" l, %lst%|+
}

CloseTab(t)
{
	glo.layouts[glo.cl].vt.RemoveAt(t)
	Loop,% glo.layouts[l].vt.MaxIndex()
		LoadTab(A_Index, glo.cl)
	GoTab(glo.ct := glo.layouts[glo.cl].vt.MaxIndex(),glo.cl)
}

ReadLayouts()
{
	glo.layouts := {}

	Loop,% A_ScriptDir "\Layouts\*.*",2
	{		
		tt := A_LoopFileName
		glo.layouts[tt] := {}
		glo.layouts[tt]["layout"] := {}
		lo := {}
		i = 0

		Loop, Read,% A_ScriptDir "\Layouts\" tt "\list"
		{
			(cl := (SubStr(A_LoopReadLine,1,1) = ",")) 
			? (ll := SubStr(A_LoopReadLine,2), lo[i][gl+=1] := {})
			: (lo[i+=1] := {}, ll := A_LoopReadLine, lo[i][gl:=1] := {})
			la := StrSplit(ll,",")
			if !(la.MaxIndex())
				la[1] := ll

			Loop,% la.MaxIndex()
			{	
				ore := {}
				re := "m)\(.*\)"
				RegExMatch(la[A_Index], re, v)
				ore.name := RegExReplace(la[A_Index], re, "")
				if !v
					v := "(W100 H1)"

				v := Trim(v,"()")
				av := StrSplit(v, A_Space)
				if !(av.MaxIndex())
					av[1] := v

				Loop,% av.MaxIndex()
					ore[SubStr(av[A_Index],1,1)] := SubStr(av[A_Index],2)
					
				lo[i][gl].Push(ore)
				log .= i " --- " gl " ---- " lo[i][gl][A_Index].W "`n"
			}
			glo.layouts[tt]["layout"] := lo
		}
		

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
	Gui, Add, ComboBox,% "w111 yp x+5 vstfolder +Choose" lll ,% glo.layouts[glo.cl]["templates"]["list"] "" 
	Gui, Add, Button, w80 xm y+5 +Default, Save
	Gui, Show,, Save Template as:

	Return

	STButtonSave:
	Gui, ST:Submit
	d := A_ScriptDir "\layouts\default\" stfolder
	f := FileOpen(d "\" stname, "w")
	f.Seek(0)
	f.Write(glo.layouts[glo.cl].vt[glo.ct].templateSave)
	f.Close()

	Loop, %d%\*.*
		s .= A_LoopFileName "|"
	glo.layouts[glo.cl]["templates"][stfolder] := s
	SetBox("TempLst","|")
	SetBox("TempLst",s)
	Return

	STGuiEscape:
	STGuiClose:
	Gui, ST:Submit
	Return

}

Templation(ttt,m="t")
{
	t := glo.ct
	l := glo.cl

	Loop ,% glo.layouts[glo.ct].MaxIndex()
	{
		balko := "`%" glo.layouts[glo.cl][layout][A_Index] "`%"
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
	For, k in glo.layouts
		Menu, Layout, Add, %k%, MenuSelecta
	Menu, TemplateMenu, Add, &Save`tCtrl+S, SaveTemplate
	Menu, TemplateMenu, Add, &Delete, BoxUpdate
	Menu, TabMenu, Add, &New`tCtrl+T, NewTab
	Menu, TabMenu, Add, &Close`tCtrl+W, CloseTab
	Menu, TabMenu, Add, &Flush`tCapsLk+X, FlushTab
	Menu, HelpMenu, Add, &About, BoxUpdate
	Menu, MyMenuBar, Add, &Tab, :TabMenu
	Menu, MyMenuBar, Add, &Layout, :Layout
	Menu, MyMenuBar, Add, &Notes, :TemplateMenu
	;Menu, MyMenuBar, Add, &Options, MenuSelecta
	;Menu, MyMenuBar, Add, &Help, MenuSelecta
	Menu, MyMenuBar, Add, E&xit, DEATH
}

MenuSelecta()
{
	men := A_ThisMenu, itm := A_ThisMenuItem

	if (men="Layout")
		SwitchLayout(itm)
	
	if (itm="&Options")
		PrefTab()
}

PrefTab()
{
	
}

SwitchLayout(itm)
{
	GuiControl, CH:MoveDraw,% "Tabbe" glo.cl, y-1000
	GuiControl, CH:MoveDraw, Tabbe%itm%, y0
	glo.cl := itm
	glo.ct := 1
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
	s := ""
	. "CASETITLE" nl
	. "MAIL" nl
	. "CASENOTES(W100 H3)" nl
	. "FIRSTNAME(W50 H1),LASTNAME(W50 H1)" nl
	. ",STREET(W75 H1),STRTNR(W25 H1)" nl
	. ",ZIP(W32 H1),CITY(W43 H1),RE(W26 H1)"

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
	glo.winH := mwaBottom
	glo.winW := (A_ScreenWidth*.2)
}

Texts:
Help = 
(
When ClippingTool window is active you can use the following hotkeys:

Alt+[0-9]: Focus box[0-1]
Ctrl+S: Save current note.
Ctrl+T: New tab.
Ctrl+W: Close tab.
Ctrl+[1-9]: GoTo tab[1-9].

The hotkeys described below are global and works even if ClippingTool window is hidden.

CapsLock: Show/Activate ClippingTool window, if it is active, activate the previously activated window.
Ctrl+CapsLock: Hide ClippingTool window.
CapsLock+[0-9]: Send content of inputbox[0-9].
Shift+CapsLock+[0-9]: Set content of inputbox[0-9] to selected text. 
CapsLock+`: Send note. 
Shift+CapsLock+`: Set note to selected text. 
CapsLock+X: Flush Tab
)

glo.txtHlp := Help
return

DEATH:
ExitApp