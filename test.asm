ORG 1000h

DATA <= {
  ports <= 00h, 00h, 00h
  arr <= 00h, 01h, 02h, 03h
}

CODE <= {
  LDX [FFh], X
  TXS 2
  BRK
}
