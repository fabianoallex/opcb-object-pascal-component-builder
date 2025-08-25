unit AutoClickButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ExtCtrls;

type
  TAutoClickButton = class(TButton)
  private
    FStartDelay: Integer;      // tempo de espera antes de começar a contagem
    FCountdown: Integer;       // tempo total da contagem regressiva (segundos)
    FRemaining: Integer;       // tempo restante na contagem
    FTimer: TTimer;            // timer interno
    FStage: Integer;           // 0 = parado, 1 = esperando, 2 = contando
    FOriginalCaption: string;  // guarda o caption original
    procedure OnTimer(Sender: TObject);
    procedure SetStartDelay(const Value: Integer);
    procedure SetCountdown(const Value: Integer);
    procedure SetButtonCaption(const Value: string);
    function GetButtonCaption: string;
  protected
    procedure Click; override; // intercepta clique manual
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;   // reinicia todo o processo
    procedure Cancel;  // cancela sem auto clique
  published
    property StartDelay: Integer read FStartDelay write SetStartDelay; // ms
    property Countdown: Integer read FCountdown write SetCountdown;    // segundos
    property Caption: string read GetButtonCaption write SetButtonCaption;
  end;

implementation

{ TAutoClickButton }

constructor TAutoClickButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimer;
  FStartDelay := 3000; // 3 segundos default
  FCountdown := 10;    // 10 segundos default
  FStage := 0;
  FOriginalCaption := inherited Caption; // guarda o inicial
end;

destructor TAutoClickButton.Destroy;
begin
  FTimer.Free;
  inherited;
end;

function TAutoClickButton.GetButtonCaption: string;
begin
  Result := FOriginalCaption;
end;

procedure TAutoClickButton.SetButtonCaption(const Value: string);
begin
  FOriginalCaption := Value;
  inherited Caption := Value;
end;

procedure TAutoClickButton.OnTimer(Sender: TObject);
begin
  case FStage of
    1: // esperando delay inicial
      begin
        FTimer.Enabled := False;
        FRemaining := FCountdown;
        FStage := 2;
        inherited Caption := Format('%s (%d)', [FOriginalCaption, FRemaining]);
        FTimer.Interval := 1000; // agora conta em segundos
        FTimer.Enabled := True;
      end;
    2: // contagem regressiva
      begin
        Dec(FRemaining);
        if FRemaining > 0 then
          inherited Caption := Format('%s (%d)', [FOriginalCaption, FRemaining])
        else
        begin
          FTimer.Enabled := False;
          inherited Caption := FOriginalCaption;
          // aqui o auto clique dispara
          inherited Click;
        end;
      end;
  end;
end;

procedure TAutoClickButton.Click;
begin
  Cancel;  // se o usuário clicar manualmente, cancela tudo
  inherited Click;
end;

procedure TAutoClickButton.Reset;
begin
  Cancel;
  if FStartDelay > 0 then
  begin
    FStage := 1;
    FTimer.Interval := FStartDelay;
    FTimer.Enabled := True;
  end
  else
  begin
    // pula direto para contagem regressiva
    FStage := 2;
    FRemaining := FCountdown;
    inherited Caption := Format('%s (%d)', [FOriginalCaption, FRemaining]);
    FTimer.Interval := 1000;
    FTimer.Enabled := True;
  end;
end;

procedure TAutoClickButton.Cancel;
begin
  FTimer.Enabled := False;
  FStage := 0;
  inherited Caption := FOriginalCaption;
end;

procedure TAutoClickButton.SetStartDelay(const Value: Integer);
begin
  FStartDelay := Value;
end;

procedure TAutoClickButton.SetCountdown(const Value: Integer);
begin
  FCountdown := Value;
end;

end.

