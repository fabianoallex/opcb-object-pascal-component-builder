unit UVirtualKeyRegistry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Graphics, fgl;

type
  TVirtualKeyDef = record
    Caption: string;
    InternalCode: string;
    MappedKeyCode: Word;
    IconResourceName: string;
    IconColor: TColor;
  end;

  TVirtualKeyDefList = specialize TFPGmap<string, TVirtualKeyDef>;

var
  KeyRegistry: TVirtualKeyDefList;

procedure RegisterKeyDef(const InternalCode, Caption: string; MappedKeyCode: Word);
function FindKeyDef(const InternalCode: string): TVirtualKeyDef;

implementation

uses
  LCLType;

procedure RegisterKeyDef(const InternalCode, Caption: string; MappedKeyCode: Word);
var
  KeyDef: TVirtualKeyDef;
begin
  KeyDef.InternalCode := InternalCode;
  KeyDef.Caption := Caption;
  KeyDef.MappedKeyCode := MappedKeyCode;
  KeyRegistry.Add(KeyDef.InternalCode, KeyDef);
end;

function FindKeyDef(const InternalCode: string): TVirtualKeyDef;
var
  i: Integer;
  Def: TVirtualKeyDef;
begin
  if KeyRegistry.Find(InternalCode, I) then
    Result := KeyRegistry.Data[I];
end;

procedure RegisterStandardKeys;
begin
  // Letras A-Z
  RegisterKeyDef('KEY_A', 'A', Ord('A'));
  RegisterKeyDef('KEY_B', 'B', Ord('B'));
  RegisterKeyDef('KEY_C', 'C', Ord('C'));
  RegisterKeyDef('KEY_D', 'D', Ord('D'));
  RegisterKeyDef('KEY_E', 'E', Ord('E'));
  RegisterKeyDef('KEY_F', 'F', Ord('F'));
  RegisterKeyDef('KEY_G', 'G', Ord('G'));
  RegisterKeyDef('KEY_H', 'H', Ord('H'));
  RegisterKeyDef('KEY_I', 'I', Ord('I'));
  RegisterKeyDef('KEY_J', 'J', Ord('J'));
  RegisterKeyDef('KEY_K', 'K', Ord('K'));
  RegisterKeyDef('KEY_L', 'L', Ord('L'));
  RegisterKeyDef('KEY_M', 'M', Ord('M'));
  RegisterKeyDef('KEY_N', 'N', Ord('N'));
  RegisterKeyDef('KEY_O', 'O', Ord('O'));
  RegisterKeyDef('KEY_P', 'P', Ord('P'));
  RegisterKeyDef('KEY_Q', 'Q', Ord('Q'));
  RegisterKeyDef('KEY_R', 'R', Ord('R'));
  RegisterKeyDef('KEY_S', 'S', Ord('S'));
  RegisterKeyDef('KEY_T', 'T', Ord('T'));
  RegisterKeyDef('KEY_U', 'U', Ord('U'));
  RegisterKeyDef('KEY_V', 'V', Ord('V'));
  RegisterKeyDef('KEY_W', 'W', Ord('W'));
  RegisterKeyDef('KEY_X', 'X', Ord('X'));
  RegisterKeyDef('KEY_Y', 'Y', Ord('Y'));
  RegisterKeyDef('KEY_Z', 'Z', Ord('Z'));

  // Dígitos 0-9
  RegisterKeyDef('KEY_0', '0', Ord('0'));
  RegisterKeyDef('KEY_1', '1', Ord('1'));
  RegisterKeyDef('KEY_2', '2', Ord('2'));
  RegisterKeyDef('KEY_3', '3', Ord('3'));
  RegisterKeyDef('KEY_4', '4', Ord('4'));
  RegisterKeyDef('KEY_5', '5', Ord('5'));
  RegisterKeyDef('KEY_6', '6', Ord('6'));
  RegisterKeyDef('KEY_7', '7', Ord('7'));
  RegisterKeyDef('KEY_8', '8', Ord('8'));
  RegisterKeyDef('KEY_9', '9', Ord('9'));

  // Teclas de função F1-F12
  RegisterKeyDef('KEY_F1', 'F1', VK_F1);
  RegisterKeyDef('KEY_F2', 'F2', VK_F2);
  RegisterKeyDef('KEY_F3', 'F3', VK_F3);
  RegisterKeyDef('KEY_F4', 'F4', VK_F4);
  RegisterKeyDef('KEY_F5', 'F5', VK_F5);
  RegisterKeyDef('KEY_F6', 'F6', VK_F6);
  RegisterKeyDef('KEY_F7', 'F7', VK_F7);
  RegisterKeyDef('KEY_F8', 'F8', VK_F8);
  RegisterKeyDef('KEY_F9', 'F9', VK_F9);
  RegisterKeyDef('KEY_F10', 'F10', VK_F10);
  RegisterKeyDef('KEY_F11', 'F11', VK_F11);
  RegisterKeyDef('KEY_F12', 'F12', VK_F12);

  // Teclas de controle
  RegisterKeyDef('KEY_ENTER', 'Enter', VK_RETURN);
  RegisterKeyDef('KEY_ESCAPE', 'Esc', VK_ESCAPE);
  RegisterKeyDef('KEY_BACKSPACE', 'Backspace', VK_BACK);
  RegisterKeyDef('KEY_TAB', 'Tab', VK_TAB);
  RegisterKeyDef('KEY_SHIFT', 'Shift', VK_SHIFT);
  RegisterKeyDef('KEY_CTRL', 'Ctrl', VK_CONTROL);
  RegisterKeyDef('KEY_ALT', 'Alt', VK_MENU);
  RegisterKeyDef('KEY_CAPSLOCK', 'Caps', VK_CAPITAL);
  RegisterKeyDef('KEY_SPACE', 'Space', VK_SPACE);

  // Navegação
  RegisterKeyDef('KEY_LEFT', '←', VK_LEFT);
  RegisterKeyDef('KEY_RIGHT', '→', VK_RIGHT);
  RegisterKeyDef('KEY_UP', '↑', VK_UP);
  RegisterKeyDef('KEY_DOWN', '↓', VK_DOWN);
  RegisterKeyDef('KEY_HOME', 'Home', VK_HOME);
  RegisterKeyDef('KEY_END', 'End', VK_END);
  RegisterKeyDef('KEY_PGUP', 'PgUp', VK_PRIOR);
  RegisterKeyDef('KEY_PGDN', 'PgDn', VK_NEXT);
  RegisterKeyDef('KEY_INSERT', 'Ins', VK_INSERT);
  RegisterKeyDef('KEY_DELETE', 'Del', VK_DELETE);

  // Teclado Numérico
  RegisterKeyDef('NUMPAD_0', 'Num 0', VK_NUMPAD0);
  RegisterKeyDef('NUMPAD_1', 'Num 1', VK_NUMPAD1);
  RegisterKeyDef('NUMPAD_2', 'Num 2', VK_NUMPAD2);
  RegisterKeyDef('NUMPAD_3', 'Num 3', VK_NUMPAD3);
  RegisterKeyDef('NUMPAD_4', 'Num 4', VK_NUMPAD4);
  RegisterKeyDef('NUMPAD_5', 'Num 5', VK_NUMPAD5);
  RegisterKeyDef('NUMPAD_6', 'Num 6', VK_NUMPAD6);
  RegisterKeyDef('NUMPAD_7', 'Num 7', VK_NUMPAD7);
  RegisterKeyDef('NUMPAD_8', 'Num 8', VK_NUMPAD8);
  RegisterKeyDef('NUMPAD_9', 'Num 9', VK_NUMPAD9);
  RegisterKeyDef('NUMPAD_ADD', 'Num +', VK_ADD);
  RegisterKeyDef('NUMPAD_SUB', 'Num -', VK_SUBTRACT);
  RegisterKeyDef('NUMPAD_MUL', 'Num *', VK_MULTIPLY);
  RegisterKeyDef('NUMPAD_DIV', 'Num /', VK_DIVIDE);
  RegisterKeyDef('NUMPAD_DECIMAL', 'Num .', VK_DECIMAL);
end;

procedure InitializeKeyRegistry;
begin
  KeyRegistry := TVirtualKeyDefList.Create;
  KeyRegistry.Sorted := True;
  RegisterStandardKeys;
end;

initialization
  InitializeKeyRegistry;

finalization
  KeyRegistry.Free;

end.

