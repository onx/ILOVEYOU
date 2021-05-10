# ILOVEYOU

> **ILOVEYOU**, sometimes referred to as **Love Bug** or **Love Letter**, was a
> [computer worm](https://en.wikipedia.org/wiki/Computer_worm) that attacked
> tens of millions of [Windows](https://en.wikipedia.org/wiki/Microsoft_Windows)
> personal computers on and after 5 May 2000 local time in the
> [Philippines](https://en.wikipedia.org/wiki/Philippines) when it started
> spreading as an email message with the subject line "ILOVEYOU" and the
> attachment "LOVE-LETTER-FOR-YOU.txt.vbs".
>
> — Wikipedia, [ILOVEYOU](https://en.wikipedia.org/wiki/ILOVEYOU)

This is a formatted version of the **ILOVEYOU** worm also known as **Love
Letter**. It includes comments which explains the routines that are used by the
worm to infect and spread itself.

## How it works

The worm is distributed primarily through email, most prominently [Microsoft
Outlook](https://en.wikipedia.org/wiki/Microsoft_Outlook) at the time. It does
so by sending an email to each of the victim's contacts, listed in their
[Address Book](https://en.wikipedia.org/wiki/Windows_Address_Book).

When executed, it infects different files in the system by writing itself to
document files, MP3s/MP2s, JPEG, and other Visual Basic scripts and changing
their extension to `.vbs`, making them executable.

It also makes it so, after having executed the script the first time, will
execute on each startup of the computer, making it very difficult to stop.

It relies on the fact that Windows will automatically execute any Visual
Basic Script files, when opened from the file explorer or from Outlook, making
it trivial for a victim to accidentally execute it.

## Disclaimer

**This program and its source files are only uploaded for educational purposes.
Do not execute this program if you do not know what it does and what the risks
are.**

## Credits

The original source code was obtained from
[Cexx.org](http://www.cexx.org/loveletter.htm) and formatted and commented by
me.
