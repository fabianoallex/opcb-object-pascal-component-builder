# Documentation

* [Overview](#overview)
    * [About the library](#about-the-library)
    * [Benefits](#benefits)
    * [License - Copyright](#license---copyright)
* [Installation and Configuration](#installation-and-configuration)
* [Classes and Interfaces](#classes-and-interfaces)
    * [Builders](#builders)
        * [IObjectBuilder](#iobjectbuilder)
            * [TObjectBuilderBase](#tobjectbuilderbase)
            * [TObjectBuilder](#tobjectbuilder)
        * [IComponentBuilder](#icomponentbuilder)
            * [TComponentBuilderBase](#tcomponentbuilderbase)
            * [TComponentBuilder](#tcomponentbuilder)
        * [IControlBuilder](#icontrolbuilder)
            * [TControlBuilderBase](#tcontrolbuilderbase)
            * [TControlBuilder](#tcontrolbuilder)
        * [IMenuBuilder](#imenubuilder)
            * [TMenuBuilderBase](#tmenubuilderbase)
            * [TMenuBuilder](#tmenubuilder)
        * [IMenuItemBuilder](#imenuitembuilder)
            * [TMenuItemBuilderBase](#tmenuitembuilderbase)
            * [TMenuItemBuilder](#tmenuitembuilder)
    * [Creators](#creators)
        * [TComponentCreator](#tcomponentcreator)
        * [TControlCreator](#tcontrolcreator)
        * [TMenuCreator](#tmenucreator)
        * [TControlCreator](#tcontrolcreator-1)
        * [TOPCBCreators](#topcbcreators)


## Overview

### About the Library

**OPCB (Object Pascal Component Builder)** is a library developed in **Object Pascal**, designed to simplify the creation and configuration of both visual and non-visual components in **Delphi** and **Lazarus** applications.  
Its goal is to reduce code complexity, eliminate repetition, and provide a fluent approach to component construction, making projects easier to maintain, scale, and evolve.

### Benefits

One of the main advantages of OPCB is the ability to create components directly in code, eliminating the need for `.dfm` (Delphi) or `.lfm` (Lazarus) files.  
This way, all logic and component configuration remain centralized in `.pas` units, offering several practical benefits:

* **Compatibility between Delphi and Lazarus:** makes it easier to port projects between the two IDEs.  
* **Simplified component replacement.**  
* **Ease of creating new components** based on existing ones without having to install them in the IDE.  
* **Reduced version control conflicts:** avoids common issues with `.dfm` or `.lfm` files in Git repositories.  
* **More organized and centralized code:** all component and layout definitions are located in one place, making maintenance and code review easier.

In summary, OPCB makes interface development faster, more consistent, and more reliable, providing flexibility and full control over component creation.

---
### License - Copyright Notice

The **OPCB (Object Pascal Component Builder)** is distributed under the **Apache License, Version 2.0 (January 2004).**

This means you are free to use, modify, and distribute the library — including in commercial projects — as long as you comply with the following conditions:

1. **Copyright and License Notice**  
   * All files in the library must retain the copyright notice and a reference to the license.

2. **Redistribution of Modifications**  
   * If you modify and redistribute the library, you must clearly indicate the changes made.

3. **No Warranty**  
   * The library is provided “as is,” without any warranty of performance or liability for damages.

**Additional Resources**  
You can find the full text of the license in the `LICENSE` file included in the repository.

The license allows usage in both **open-source and proprietary projects**, providing maximum flexibility for integration in different contexts.

---
### Installation and Configuration

#### 🔹 Option 1 – Clone the Repository

```bash
git clone https://github.com/fabianoallex/opcb-object-pascal-component-builder.git
```

#### 🔹 Option 2 – Manual Download

Download the ZIP file from the GitHub repository and extract its contents to a folder of your choice.

#### 💠 Delphi

1. Open **Delphi**.  
2. Go to **Tools ▸ Options ▸ Language ▸ Delphi Options ▸ Library**.  
3. In the **Library Path** field, add the full path to the `src` folder.  
   Example:  
   `C:\opcb\src`

#### 💠 Lazarus

1. Open **Lazarus**.  
2. Go to **Project ▸ Project Options ▸ Compiler Options ▸ Paths**.  
3. In **Other unit files (-Fu)**, add the path to the `src` folder.  
   Example:  
   `C:\opcb\src`

### 🧰 All Set!

After completing these steps, the units from the OPCB project will be available for use in your Delphi or Lazarus projects.

---

### Classes and Interfaces

#### Builders

Builders are classes designed to simplify object instantiation.

![Class Diagram](img/builder-class-diagram.png)

##### IObjectBuilder

Base interface that defines operations for constructing generic objects.

| Method | Return | Description |
|--------|----------|-------------|
| `Build: TBuild` | `TBuild` | Creates and returns a new instance of the generic type `TBuild`. |
| `AssignReference(var AReference: TBuild)` | `void` | Assigns the instance created by `.Build` to the provided variable. |

---
#### `TObjectBuilderBase`

Abstract class that implements the `IObjectBuilder<TBuild>` interface  
and provides the core infrastructure for object construction using the Builder pattern.  
It is responsible for creating, configuring, and preparing instances of the generic type `TBuild`, enabling fluent methods and custom configuration operations.

| Method | Return | Description |
|--------|----------|-------------|
| `CreateObject: TBuild; virtual; abstract;` | `TBuild` | Abstract method to be implemented by derived classes. It must create a new instance using the constructor defined in `ObjectClass`. |
| `ConfigureObject(AObject: TBuild); virtual;` |  | Assigns the properties defined by the builder to the instantiated object after `.Build`. Should be overridden by classes extending `TObjectBuilderBase`, but must always call `inherited ConfigureObject(AObject);`. |
| `ApplyPropPath(Instance: TObject; AProp: TPropertyValue);` |  | Applies to the object instantiated via `.Build` a property value using RTTI and a hierarchical path (`'Prop.SubProp'`). Example: `'Font.Size'`. |
| `ApplyPendindProps(Instance: TObject);` |  | Applies to the object instantiated with `.Build` all pending properties stored in `FProperties` via RTTI. |
| `SetupProc(AProcObj: TSetupProcObj<TBuild>); overload;` |  | Adds a reference to a user-defined object setup procedure. The procedure is executed during the `.Build` method execution. |
| `SetupProc(ARefProc: TSetupRefProc<TBuild>); overload;` |  | Adds a reference to a user-defined object setup procedure. The procedure is executed during the `.Build` method execution. |
| `Create; overload;` |  | Constructor. |
| `Create(AClass: TObjectClass); overload;` |  | Constructor that takes as parameter the class to be instantiated. |
| `Create(AClass: TObjectClass; out Reference); overload;` |  | Constructor that takes the class to instantiate and an additional variable reference to receive the created instance. |
| `Destroy; override;` |  | Destructor. |
| `AssignReference(out Reference);` |  | Assigns the object created by `.Build` to the given parameter and keeps the reference in a list. |
| `ResetReferences;` |  | Clears the reference list. |
| `Assign(out Reference): TSelf; overload;` | `TSelf` | Assigns the reference and returns the instance itself for fluent usage. |
| `WithProp(const APropName: string; const AValue: TValue): TSelf; overload;` | `TSelf` | Defines a simple property value using RTTI. |
| `WithProp(const APropValue: TPropertyValue): TSelf; overload;` | `TSelf` | Defines an object property using RTTI and a `TPropertyValue` record. |
| `WithPropObj(const APropName: string; AObj: TObject): TSelf; overload;` | `TSelf` | Defines a property using RTTI. |
| `WithPropSet(const APropName: string; const AValue: Integer): TSelf;` | `TSelf` | Defines a property of type `set` using RTTI. |
|`WithEvent(const AEventName: string; const AMethod: TMethod): TSelf; overload;`|`TSelf`|Defines an object event using RTTI.|
|`WithEvent(const AEventName: string; const AInstance: TObject; const AMethod: Pointer): TSelf; overload;`|`TSelf`|Defines an object event using RTTI.|
| `Setup(AProc: TSetupProcObjBuild): TSelf; overload;` | `TSelf` | Adds an object setup procedure. |
| `Setup(AProc: TSetupRefProcBuild): TSelf; overload;` | `TSelf` | Adds an object setup procedure. |
| `Build: TBuild;` | `TBuild` | Creates, configures, and returns the final object of type `TBuild`. |
| `property ObjectClass: TObjectClass` |  | Returns the class associated with the current construction. |

---

##### `TObjectBuilder`

Concrete class that inherits from `TObjectBuilderBase`.  
Defines the generic type `TBuild` as `TComponent`.

Example of use:

```pascal
uses
  OPCB;
  
type
  TMyClass = class
  private
    FBar: string;
    FFoo: Integer;
    procedure SetBar(const Value: string);
    procedure SetFoo(const Value: Integer);
  published
    property Foo: Integer read FFoo write SetFoo;
    property Bar: string read FBar write SetBar;
  end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Builder: TObjectBuilder;
  MyObject: TMyClass;
begin
  Builder := TObjectBuilder.Create(TMyClass);

  try
    MyObject := Builder
      .WithProp('Foo', 10)
      .WithProp('Bar', 'bar')
      .Build as TMyClass;
  finally
    Builder.Free;
  end;

  with MyObject do
    ShowMessage(Format('%d - %s', [Foo, Bar]));
end;
```

---
#### `TObjectBuilder`

Concrete class that inherits from `TObjectBuilderBase<TObject, TObjectBuilder>`.

Provides a generic implementation for building instances of `TObject`, allowing the creation of objects of any class derived from `TObject` without the need to define a specific generic type.

It is useful as a universal builder for scenarios where the concrete object type is determined at runtime.

Inheritance

    TObjectBuilder = class(TObjectBuilderBase<TObject, TObjectBuilder>)

| Method | Return | Description |
|--------|----------|------------|
| `CreateObject: TObject; override;` | `TObject` | Creates a new instance of type `TObject` based on the class defined in `FObjectClass`. This method overrides the abstract implementation of the base class. |
| `Create; overload;` | | Creates a new instance of TObjectBuilder associated with the class `TObject`. |
| `Create(out Reference); overload;` | | Creates a new instance and assigns the reference of the built object to the provided variable. |
| `Create(AClass: TObjectClass); overload;` | | Initializes the builder by associating it with a specific object class (`TObjectClass`). |
| `Create(AClass: TObjectClass; out Reference); overload;` | | Creates an instance associated with a class and already assigns the output reference. |

---

#### `IComponentBuilder`

The `IComponentBuilder<TBuild>` interface extends `IObjectBuilder<TBuild>`
 and defines specific operations for building components (`TComponent` and descendants).

It adds support for managing common component properties — such as `Name`, `Owner`, and `Tag` — allowing the creation process to be fully controlled in a fluent and consistent manner.

Inheritance

    IComponentBuilder<TBuild> = interface(IObjectBuilder<TBuild>)

| Property | Type | Description |
|--------|----------|------------|
| `Name` | `string` | Defines the name of the component, equivalent to the Name property of TComponent. |
| `Tag` | `NativeInt` | Defines the value of the free identifier (Tag) associated with the component. |
| `Owner` | `TComponent` | Defines or gets the component responsible for managing the lifecycle of the built component. |

Methods

| Method | Type | Description |
|--------|----------|------------|
| `GetName` | `string` | Returns the name to be assigned to the object to be instantiated. |
| `SetName(AValue: string);` | | Defines the name of the component to be instantiated by `.Build`. |
| `GetTag` | `NativeInt` | Returns the value of the `Tag` property to be assigned to the instantiated object. |
| `SetTag(AValue: NativeInt);` | | Defines the value of the `Tag` property of the object to be instantiated. |
| `GetOwner` | `TComponent` | Returns the `Owner` associated with the component to be instantiated. |
| `SetOwner(AValue: TComponent);` | | Defines the `Owner` responsible for the lifecycle of the component to be instantiated. |

---

#### `TComponentBuilderBase`

Abstract class that extends `TObjectBuilderBase<TBuild, TSelf>`
 and implements the `IComponentBuilder<TBuild>` interface.

Serves as the base for all builders focused on creating and configuring components (TComponent and descendants), adding support for typical properties such as `Name`, `Owner`, and `Tag`.

Furthermore, `TComponentBuilderBase` maintains the fluency of the methods inherited from the base class (`TObjectBuilderBase`), preserving the return of the generic type `TSelf`.
This allows chaining calls between inherited methods and specific component methods, ensuring a fluent experience, for example:

```pascal
Builder
  .WithName('BtnOk')
  .WithTag(10)
  .WithProp('Caption', 'Confirm')
  .Setup(SetupProc)
  .Build;
```

Inheritance

    TComponentBuilderBase<TBuild, TSelf> = class(TObjectBuilderBase<TBuild, TSelf>, IComponentBuilder<TBuild>)

Protected fields
| Field | Type | Description |
|--------|----------|------------|
| `FName` | `string` | Stores the name of the component. |
| `FTag` | `NativeInt` | Stores the free identification value of the component. |
| `FOwner` | `TComponent` | Stores the component that will be the owner of the created instance. |

Protected methods
| Method | Description |
|----------|------------|
| `ConfigureObject(AObject: TBuild); override;` | Performs additional component configuration after its creation, applying `Name`, `Tag`, and `Owner` as defined. |

Public methods
| Method | Type | Description |
|--------|----------|------------|
| `Create(AClass: TComponentClass; const AName: string=''); overload;` | | Constructor. Creates the builder for a component type and optionally sets the initial name. |
| `Create(AClass: TComponentClass; const AName: string; out Reference); overload;` | | Constructor. Creates the builder for a component type, sets the name, and assigns the reference of the created object. |
| `Create(AClass: TComponentClass; out Reference); overload;` | | Constructor. Creates the builder and assigns the reference without setting the name. |
| `GetName` | `string` | Returns the current name configured for the component. |
| `GetOwner: TComponent;` | | Returns the owner currently associated with the component. |
| `GetTag` | `NativeInt` | Returns the current value configured for the `Tag` property. |
| `SetName(AValue: string);` | | Defines the name of the component that will be assigned during construction. |
| `SetOwner(AValue: TComponent);` | | Defines the owner responsible for the component's lifecycle. |
| `SetTag(AValue: NativeInt);` | | Defines the value of the component's `Tag` property. |
| `WithName(AName: string)` | `TSelf` | Defines the component's name and returns the own instance (fluent method). |
| `WithTag(ATag: NativeInt)` | `TSelf` | Defines the value of the `Tag` property and returns the own instance (fluent method). |

Properties
| Property | Type | Description |
|--------|----------|------------|
| `Name` | `string` | Name of the component to be created. |
| `Tag` | `NativeInt` | Numeric value associated with the component. |
| `Owner` | `TComponent` | Owner component responsible for the created instance. |

---

#### `TComponentBuilder`

The `TComponentBuilder` class is a concrete specialization of the generic `TComponentBuilderBase` class, designed to facilitate the fluent and configurable creation of instances of `TComponent` (or any descendant).

It provides overloaded constructors that allow:

* Creating the component without a name or reference.
* Creating the component with a defined name.
* Creating the component by automatically assigning the instance to an external variable.

The protected method `CreateObject` is overridden to perform the actual instantiation of the component, using the type specified in the constructor.

Like the other Builder classes in the library, `TComponentBuilder` maintains the fluency inherited from the base class methods (`TComponentBuilderBase`), allowing method chaining with return of type `TSelf`.

This means that inherited and specific methods can be freely combined, providing a readable and expressive construction.

| Constructor | Description |
|--------|------------|
| `Create` | Creates the builder without parameters, using `TComponent` as the class to be instantiated. |
| `Create(AClass: TComponentClass; const AName: string = '')` | Creates the builder for the specified class, optionally setting the component's name. |
| `Create(AClass: TComponentClass; const AName: string; out Reference)` | Creates the builder and assigns the created instance to the `Reference` variable. |
| `Create(AClass: TComponentClass; out Reference)` | Creates the builder and assigns the created instance to the `Reference` variable, without setting a name. |

Method
| Method | Type | Description |
|--------|----------|------------|
| `CreateObject: TComponent` | `TComponent` | Overrides the base method to instantiate the component specified in `AClass`. |

---  
#### `IControlBuilder`

The `IControlBuilder<TBuild>` interface extends `IComponentBuilder<TBuild>`
, adding support for typical properties and events of visual controls (`TControl` and descendants).

It defines access methods (`get/set`) and optional properties that allow dynamically configuring visual aspects, such as position, dimensions, alignment, text, and events, without the need for direct interaction with the form or .dfm/.lfm files.

The use of optional types (`TOptionalString`, `TOptionalSingle`, `TOptionalAlign`) allows distinguishing between explicitly set values and default values inherited from the control, making the builder safer and more predictable in partial initialization scenarios.

**Main properties**
| Property | Type | Description |
|--------|----------|------------|
| `Caption` | `TOptionalString` | Defines the display text of the control (when applicable). |
| `Text` | `TOptionalString` | Defines the textual content, used in input controls like `TEdit` or `TMemo`. |
| `Align` | `TOptionalAlign` | Defines the alignment of the control within the container (`alTop`, `alLeft`, etc.). |
| `Width` | `TOptionalSingle` | Width of the control in pixels. |
| `Height` | `TOptionalSingle` | Height of the control in pixels. |
| `Top` | `TOptionalSingle` | Vertical position relative to the container. |
| `Left` | `TOptionalSingle` | Horizontal position relative to the container. |
| `Parent` | `TWinControl` | Parent container where the control will be inserted. |
| `OnClick` | `TNotifyEvent` | Defines the click event of the control. |

**Defined methods**
| Method | Description |
|--------|---------------|
| `Get/SetAlign` | Gets or sets the alignment of the control. |
| `Get/SetCaption` | Gets or sets the display text. |
| `Get/SetText` | Gets or sets the textual content of the control. |
| `Get/SetHeight` | Gets or sets the height. |
| `Get/SetWidth` | Gets or sets the width. |
| `Get/SetTop` | Gets or sets the vertical position. |
| `Get/SetLeft` | Gets or sets the horizontal position. |
| `Get/SetParent` | Defines the parent container (`TWinControl`). |
| `Get/SetOnClick` | Defines the `OnClick` event handler. |

---

#### `TControlBuilderBase`

The `TControlBuilderBase<TBuild, TSelf>` class is an abstract class that implements `IControlBuilder<TBuild>` and extends `TComponentBuilderBase<TBuild, TSelf>`.

It provides a fluent and generic implementation for building and configuring visual controls (`TControl` and descendants), both in Delphi and Lazarus.

Through chainable methods (`With`), it's possible to define properties such as position, size, alignment, text, and events without depending on forms or .dfm/.lfm files.
This class maintains the call fluency inherited from the base hierarchy — all methods return TSelf, allowing compositions like:
```pascal
TControlBuilder.Create(TButton)
  .WithName('BtnSave')
  .WithCaption('Save')
  .WithAlign(alBottom)
  .WithOnClick(@OnSaveClick)
  .Build;
```

**Main properties**
| Property | Type | Description |
|--------|--------|---------------|
| `Parent` | `TWinControl` | Defines the container where the control will be inserted. |
| `Caption` | `TOptionalString` | Display text of the control (when applicable). |
| `Text` | `TOptionalString` | Textual content (ex.: `TEdit.Text`). |
| `Align` | `TOptionalAlign` | Alignment of the control in the container (`alTop`, `alClient`, etc.). |
| `Width` | `TOptionalSingle` | Width of the control in pixels. |
| `Height` | `TOptionalSingle` | Height of the control in pixels. |
| `Top` | `TOptionalSingle` | Vertical position relative to the container. |
| `Left` | `TOptionalSingle` | Horizontal position relative to the container. |
| `OnClick` | `TNotifyEvent` | Event triggered when the control is clicked. |

**Configuration methods (fluent)**
| Method | Description |
|--------|---------------|
| `WithAlign(AAlign)` | Defines the alignment (`TAlign` or `TAlignLayout`, according to the framework). |
| `WithName(AName)` | Defines the name (`Name`) of the control. |
| `WithTag(ATag)` | Defines the numeric identifier (`Tag`). |
| `WithWidth(AWidth)` | Defines the width. |
| `WithHeight(AHeight)` | Defines the height. |
| `WithWidthAndHeight(AWidth, AHeight)` | Defines width and height simultaneously. |
| `WithTop(ATop)` | Defines the vertical position. |
| `WithLeft(ALeft)` | Defines the horizontal position. |
| `WithCaption(ACaption)` | Defines the display text (`Caption`). |
| `WithText(AText)` | Defines the textual content (`Text`). |
| `WithOnClick(AOnClick)` | Defines the click event (`OnClick`). |

**Inherited and overridden methods**
| Method | Description |
|--------|---------------|
| `ConfigureObject(AObject: TBuild)` | Overrides the base method to apply the configured visual properties. |
| `Create` | Default constructor, initializes the instance by defining `TControl` as the Class to be instantiated. |
| `Get/Set*` | Implementations of the accessors of the `IControlBuilder<TBuild>` interface. |

**Notes**

* Maintains full fluency with the methods of the base class (TComponentBuilderBase and TObjectBuilderBase), thanks to the use of the generic type TSelf.
* Compatible with Delphi (VCL/FMX) and Lazarus (LCL), adapting the Align type according to the active framework (`TAlign` or `TAlignLayout`).
* Allows components to be created entirely via code, reducing dependency on visual forms.
* It is the base for concrete builders like TControlBuilder and other specialized builders (`TButtonBuilder`, `TPanelBuilder`, etc.).

---
#### `TControlBuilder`

The `TControlBuilder` class is the concrete implementation of `TControlBuilderBase<TBuild, TSelf>`, specialized for the TControl type.

It serves as a universal builder for visual controls, allowing dynamic instantiation of any class descending from TControl (such as `TButton`, `TPanel`, `TEdit`, etc.) with fluent configuration.

Structure

```pascal
TControlBuilder = class(TControlBuilderBase<TControl, TControlBuilder>)
protected
  function CreateObject: TControl; override;
public
  constructor Create; overload;
  constructor Create(AClass: TControlClass; const AName: string=''); overload;
  constructor Create(AClass: TControlClass; const AName: string; out Reference); overload;
  constructor Create(AClass: TControlClass; out Reference); overload;
end;
```

**Description**

The class implements the `CreateObject` method, responsible for instantiating the control specified in the constructor. It is ideal for scenarios where you want to create and configure controls without needing to declare a specific builder class, maintaining the flexibility and fluent style of the OPCB library.

Main Constructors
| Method | Description |
|--------|---------------|
| `Create` | Creates a generic builder, defining the instantiation class as `TControl`. |
| `Create(AClass: TControlClass; const AName: string='')` | Creates a builder for the specified class (`TButton`, `TPanel`, etc.), optionally assigning a name. |
| `Create(AClass: TControlClass; const AName: string; out Reference)` | Same as above, but returns the reference of the created control. |
| `Create(AClass: TControlClass; out Reference)` | Variant without an initial name, but with return by reference. |

**Example of use**
```pascal
uses
  OPCB.ControlBuilder;

var
  Btn: TButton;
begin
  TControlBuilder.Create(TButton, 'BtnOk', Btn)
    .WithCaption('OK')
    .WithAlign(alRight)
    .WithWidthAndHeight(100, 30)
    .WithOnClick(@OnOkClick)
    .Build;
end;
```

**Notes**

* Maintains complete fluency with the methods inherited from TControlBuilderBase
 and previous classes in the hierarchy.
* Avoids the need for specific classes when there is no additional behavior to override.
* Ideal for dynamic UI creation in compatible frameworks (VCL, LCL, and FMX).

---
#### `IMenuBuilder`

The `IMenuBuilder\<TBuild\>` interface extends `IComponentBuilder\<TBuild\>`
, providing the necessary structure for builders aimed at components of type `TMenu` (such as `TMainMenu`, `TPopupMenu`, and derivatives).

Currently, it does not add new members beyond the inherited ones, but serves as an extension point for future specializations.

**Note**

Even without declaring additional methods, the use of a separate interface brings benefits of generic typing and semantics, allowing other builders and creators to recognize menus in a specific way (for example, `TMenuItemBuilder`).

---

#### `TMenuBuilderBase`

The abstract class `TMenuBuilderBase\<TBuild, TSelf\>` is the base implementation of the `IMenuBuilder\<TBuild\>` interface.
It inherits all the infrastructure from TComponentBuilderBase
, maintaining the fluent style and the configuration and construction mechanisms already standardized in the library hierarchy.

---

#### `TMenuBuilder`

The TMenuBuilder class is the concrete implementation of TMenuBuilderBase<TBuild, TSelf>, specialized for the TMenu type.

| Constructor | Usage |
| -------------------------------------- | ------------------------------------------------------------------ |
| `Create` | Creates the builder with the default type `TMenu`. |
| `Create(AClass: TMenuClass)` | Creates a menu of a specific class (for example, `TPopupMenu`). |
| `Create(AClass, AName)` | Defines the component's name when creating it. |
| `Create(AClass, AName, out Reference)` | Creates and already provides a reference for external use. |
| `Create(AClass, out Reference)` | Creates with reference, without a name. |

---

#### `IMenuItemBuilder`

This interface defines the contract for building menu items, such as `TMenuItem`.

| Property | Type | Function |
| ------------ | ------------------ | --------------------------------------------------------------- |
| `Caption` | `TOptionalString` | Defines the text displayed in the menu item. |
| `ImageIndex` | `TOptionalInteger` | Defines the index of the associated image, if there is a `TImageList`. |
| `OnClick` | `TNotifyEvent` | Defines the click event of the item. |

---

#### `TMenuItemBuilderBase`

`TMenuItemBuilderBase` provides a generic base for fluent construction of menu items (`TMenuItem`), inheriting all the component creation and configuration infrastructure offered by `TComponentBuilderBase`.

| Field | Type | Description |
| ------------- | ------------------ | ------------------------------------------- |
| `FCaption` | `TOptionalString` | Stores the text of the menu item. |
| `FImageIndex` | `TOptionalInteger` | Defines the index of the associated image. |
| `FOnClick` | `TNotifyEvent` | Stores the click event handler. |

**Methods**

| Method | Description |
| ----------------------------- | ----------------------------------------- |
| `WithCaption(ACaption)` | Defines the text to be displayed. |
| `WithImageIndex(AImageIndex)` | Defines the index of the associated image. |
| `WithOnClick(AOnClick)` | Defines the `OnClick` event handler. |

---

#### `TMenuItemBuilder`

`TMenuItemBuilder` is the concrete and ready-to-use implementation of the builder for `TMenuItem` objects and descendants.
It inherits all the configuration logic from the base class `TMenuItemBuilderBase`, offering a simple and straightforward entry point for fluent creation of menu items in Delphi or Lazarus.

The class follows the same hierarchy as the other builders in the library:
```
TObjectBuilderBase
  └── TComponentBuilderBase
        └── TMenuItemBuilderBase
              └── TMenuItemBuilder ← implementação concreta
```


**Constructors**

| Constructor | Typical usage |
| -------------------------------------- | ------------------------------------------------------------------- |
| `Create` | Standard creation with `TMenuItem` as the base class. |
| `Create(AClass: TMenuItemClass)` | Allows building descendants of `TMenuItem` (e.g., `TMenuItemEx`). |
| `Create(AClass, AName)` | Defines the design-time name when instantiating. |
| `Create(AClass, AName, out Reference)` | Creates and returns the reference of the instantiated object. |
| `Create(AClass, out Reference)` | Creates and returns a reference without a predefined name. |

---


#### TComponentCreator

`TComponentCreator` allows centralizing the creation of multiple components and maintaining an internal registry of these component instances, enabling their retrieval later.

**Private Fields**

| Field                   | Type                     | Description                                                                 |
| ------------------------ | ------------------------ | --------------------------------------------------------------------------- |
| `FOwner`                 | `TComponent`             | Defines the owner of the components added via the `Add` method.             |
| `FRegistryContextHandle` | `IRegistryContextHandle` | Indicates the ContextHandle used to register created components. Allows sharing the registry with other `Creators`, such as `TControlCreator`, `TMenuCreator`, etc. |

**Private Methods**

| Method                        | Returns                | Description                                                                 |
| ------------------------------ | ---------------------- | --------------------------------------------------------------------------- |
| `GetComponentRegistry`         | `TComponentRegistry`   | Returns the component registry from the `FRegistryContextHandle`.           |
| `GetComponents`                | `TComponentList`       | Returns the list of components linked to the registry from the `FRegistryContextHandle`. |
| `GetItem(const AName: string)` | `TComponent`           | Retrieves a component from the list by name. The component must have been assigned a name. |

**Constructors**

| Constructor | Typical Usage |
| ------------ | -------------- |
| `Create(ARegistryContextKey: string='')` | Instantiates a `TComponentCreator`. If `ARegistryContextKey` is provided, it will use an existing registry with the same name; otherwise, a new one will be created. If no key is provided, a unique random key will be generated, creating a new registry. |
| `Create(ARegistryContextHandle: IRegistryContextHandle)` | Instantiates a `TComponentCreator` using an existing component registry retrieved through `ARegistryContextHandle`. |

**Public Methods**

| Method | Returns | Description |
| ------- | -------- | ------------ |
| `External(const AProc: TComponentCreatorObjProc)` | `TComponentCreator` | Allows external code injection through a procedure of type `TComponentCreatorObjProc`. |
| `External(const AProc: TComponentCreatorProc)` | `TComponentCreator` | Allows external code injection through a procedure of type `TComponentCreatorProc`. |
| `GetComponent<T: TComponent>(const AName: string): T; overload;` | `T` | Generic method that attempts to return a component by its name. |
| `GetComponent(const AName: string): TComponent;` | `TComponent` | Method that attempts to return a component by its name. |
| `SetOwner(AOwner: TComponent)` | `TComponentCreator` | Fluent method that sets the owner used for components added via `.Add`. |
| `Add(AComponentBuilder: IComponentBuilder<TComponent>)` | `TComponentCreator` | Fluent method that adds a new component using a `TComponentBuilder`. |

**Properties**

This interface defines the contract for building menu items such as `TMenuItem`.

| Property | Type | Purpose |
| --------- | ---- | -------- |
| `Registry` | `TComponentRegistry` | Returns the `TComponentRegistry` used to store the created components. |
| `Items[const AName: string]` | `TComponent` | Returns a component from the registry by its name. |

**Example**
```pascal
var
  Creator: TComponentCreator;
  CDS: TClientDataSet;
  DS: TDataSource;
begin
  Creator := TComponentCreator.Create;

  try
    Creator
      .SetOwner(Self)
      .Add(TComponentBuilder.Create(TClientDataSet, CDS))
      .Add(TComponentBuilder.Create(TDataSource, DS))
    ;

    DS.DataSet := CDS;
  finally
    Creator.Free;
  end;
end;
