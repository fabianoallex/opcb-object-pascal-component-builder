# Documentação

* [Visão geral](#visão-geral)
    * [Sobre a biblioteca](#sobre-a-biblioteca)
    * [Benefícios](#benefícios)
    * [Licença - Direitos autorais](#licença---direitos-autorais)
* [Instalação e configuração](#instalação-e-configuração)
    * [Instalação](#instalação)
        * [Delphi](#delphi)
        * [Lazarus](#lazarus)
    * [Exemplo](#exemplo)
* [Classes e Interfaces](#classes-e-interfaces)
    * [Builders](#builders)
        * [IObjectBuilder](#iobjectbuilder)
            * [TObjectBuilderBase](#tobjectbuilderbase)
            * [TObjectBuilder](#tobjectbuilder)
        * [IComponentBuilder](#icomponentbuilder)
            * [TComponentBuilderBase](#tcomponentbuilderbase)
            * [TComponentBuilder](#tcomponentbuilder)
        * [IMenuBuilder](#imenubuilder)
            * [TMenuBuilderBase](#tmenubuilderbase)
            * [TMenuBuilder](#tmenubuilder)
        * [IMenuItemBuilder](#imenuitembuilder)
            * [TMenuItemBuilderBase](#tmenuitembuilderbase)
            * [TMenuItemBuilder](#tmenuitembuilder)
        * [IControlBuilder](#icontrolbuilder)
            * [TControlBuilderBase](#tcontrolbuilderbase)
            * [TControlBuilder](#tcontrolbuilder)
    * [Creators](#creators)
        * [TComponentCreator](#tcomponentcreator)
        * [TControlCreator](#tcontrolcreator)
        * [TMenuCreator](#tmenucreator)
        * [TControlCreator](#tcontrolcreator-1)
        * [TOPCBCreators](#topcbcreators)


## Visão geral

### Sobre a biblioteca

A OPCB (Object Pascal Component Builder) é uma biblioteca desenvolvida em Object Pascal, criada para simplificar a criação e configuração de componentes visuais e não-visuais em aplicações Delphi e Lazarus.
Seu objetivo é reduzir a complexidade do código, eliminar repetição e fornecer uma abordagem fluente para a construção de componentes, tornando os projetos mais fáceis de manter, escalar e evoluir.

### Benefícios

Uma das principais vantagens da OPCB é permitir a criação de componentes diretamente no código, dispensando o uso de arquivos .dfm (Delphi) ou .lfm (Lazarus). Dessa forma, toda a lógica e configuração dos componentes ficam centralizadas em unidades .pas, o que oferece vários benefícios práticos:

* Compatibilidade entre Delphi e Lazarus: facilita a portabilidade de projetos entre as duas IDEs.
* Substituição de componentes simplificada: permite substituição de Componentes;
* Facilidade de criar novos componentes com base nos existentes sem precisar instalá-los na IDE.
* Redução de conflitos em sistemas de versionamento: evita problemas comuns com arquivos .dfm ou .lfm em repositórios Git.
* Código mais organizado e centralizado: todas as definições de componentes e layouts ficam em um único lugar, facilitando manutenção e revisão.

Em resumo, a OPCB torna o desenvolvimento de interfaces mais rápido, consistente e confiável, oferecendo flexibilidade e controle total sobre a criação de componentes.

---

### Licença - Direitos autorais

A OPCB (Object Pascal Component Builder) é distribuída sob a Apache License, Version 2.0 (Janeiro de 2004).

Isso significa que você pode usar, modificar e distribuir a biblioteca livremente, inclusive em projetos comerciais, desde que respeite os seguintes pontos:

1. **Notificação de copyright e licença**
    * Todos os arquivos da biblioteca devem manter a indicação de copyright e a referência à licença.
2. **Redistribuição de modificações**
    * Se você alterar a biblioteca e redistribuí-la, deve informar as mudanças realizadas.
3. **Sem garantias**
    * A biblioteca é fornecida “no estado em que se encontra”, sem qualquer garantia de funcionamento ou responsabilidade por danos.

**Recursos adicionais**

Você pode consultar o texto completo da licença no arquivo LICENSE do repositório.

A licença permite tanto uso em projetos open source quanto proprietários, oferecendo flexibilidade máxima para integração em diferentes contextos.
