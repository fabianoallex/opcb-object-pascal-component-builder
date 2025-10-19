# Documentação

* [Visão geral](#visao-geral)
    * [Sobre a biblioteca](#sobre-a-biblioteca)
    * [Características](#características)
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


## Visao geral

### Sobre a biblioteca
A **OPCB (Object Pascal Component Builder)** é uma biblioteca desenvolvida em Object Pascal, projetada para simplificar a criação e configuração de componentes visuais e não-visuais e menus em aplicações Delphi e Lazarus.  
Seu principal objetivo é reduzir a complexidade do código, minimizar repetição e fornecer uma abordagem fluente para a construção de componentes, facilitando tanto a manutenção quanto a escalabilidade de projetos.

A biblioteca é adequada para desenvolvedores que trabalham com interfaces gráficas, sistemas de automação, aplicações empresariais e qualquer projeto onde a criação dinâmica e estruturada de componentes seja necessária.

---