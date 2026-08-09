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
        * [TButtonBuilder](#tbuttonbuilder)
    * [Creators](#creators)
        * [TComponentCreator](#tcomponentcreator)
        * [TControlCreator](#tcontrolcreator)
            * [TControlCreatorSiblingSubLevelOptions](#tcontrolcreatorsiblingsubleveloptions)
        * [TMenuCreator](#tmenucreator)
        * [TOPCBCreators](#topcbcreators)
    * [Registry](#registry)
        * [TComponentRegistry](#tcomponentregistry)


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

> **`With*` means different things on Builders vs. on Creators.** On **Builders** (`TObjectBuilderBase` and descendants), `With*` methods (`WithOwner`, `WithName`, `WithTag`, `WithParent`, ...) are the permanent, non-deprecated way to configure a not-yet-built object — they stay `With*` on purpose, because the non-fluent `SetOwner`/`SetName`/`SetTag` setters required by the `IComponentBuilder<T>`/`IControlBuilder<T>` interfaces already occupy those exact names (a fluent `function SetOwner(...): TSelf` would collide with the existing `procedure SetOwner(...)` — same parameters, only the return type differs, which Object Pascal doesn't accept as an overload). On **Creators** (`TComponentCreator`, `TControlCreator`, `TMenuCreator`), most `With*` methods are deprecated in favor of a `Set*` counterpart (`WithOwner` → `SetOwner`, `WithOwnerAndParent` → `SetOwnerAndParent`, `WithParent` → `SetParent`) — there the non-fluent-setter collision doesn't apply, so the library moved to the (arguably clearer) `Set*` convention going forward. In short: on a **Builder**, keep using `With*`; on a **Creator**, prefer `Set*` — the compiler will flag the deprecated ones either way.

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
| `DiscardReferences;` |  | Sets every `out Reference` variable registered via `.Assign`/the `out Reference` constructor overloads back to `nil`, **without** building anything. Used when the caller decides not to build after all (e.g. `TControlCreator.Add` when the grid is already full) and also internally when `.Build` raises after the object was already created — either way, it prevents an `out Reference` variable from being left pointing at stale/garbage data or at an object that's about to be freed. |
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
#### `TButtonBuilder`

`TButtonBuilder` is a specialized builder for `TButton` (and descendants), declared in the separate `OPCB.Builders` unit rather than the core `OPCB` unit. It adds two button-specific fluent properties on top of everything `TControlBuilderBase` already provides.

Inheritance

    TButtonBuilder = class(TControlBuilderBase<TButton, TButtonBuilder>)

**Constructors**

| Constructor | Description |
|--------------|-------------|
| `Create(const AName: string = '')` | Creates the builder using `TButton` itself as the class to instantiate. |
| `Create(AClass: TButtonClass; const AName: string = '')` | Creates the builder for a specific `TButton` descendant class. |
| `Create(AClass: TButtonClass; const AName: string; out Reference)` | Same as above, and assigns the created instance to `Reference`. |
| `Create(AClass: TButtonClass; out Reference)` | Same as above, without setting an initial name. |

**Fluent Methods**

| Method | Return | Description |
|--------|--------|-------------|
| `WithModalResult(AModalResult: Integer)` | `TButtonBuilder` | Sets the button's `ModalResult` (e.g. `mrOk`, `mrCancel`) — commonly used for dialog buttons. |
| `WithEnabled(AEnabled: Boolean)` | `TButtonBuilder` | Sets the button's `Enabled` state. |

**Example**
```pascal
Creator.Add(TButtonBuilder.Create(TButton, 'BtnOk')
  .WithCaption('OK')
  .WithModalResult(mrOk)
);
```

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
| `WithOwner(AOwner: TComponent)` ⚠️ *deprecated* | `TComponentCreator` | Deprecated, use `.SetOwner` instead. |
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
```

#### TControlCreator

TControlCreator allows centralizing the creation of multiple Controls and maintaining an internal registry of created instances, making it possible to retrieve them later.

TControlCreator was designed to facilitate the creation of visual controls and position them on the screen without the need to drag components on the screen through the IDE at design time.

**Private Fields**

| Field | Type | Description |
|-------|------|-------------|
| `FOwner` | `TComponent` | Defines the *Owner* of controls added through the `.Add` method. |
| `FRegistryContextHandle` | `IRegistryContextHandle` | Indicates the *ContextHandle* used to register the created components. Allows sharing the registry with other *Creators*, such as `TControlCreator`, `TMenuCreator`, etc. |
| `FGroups` | `TControlGroupMap` | `TControlGroupMap = TDictionary<string, TControlList>;`. <br> Allows adding controls to specific groups for later retrieval based on their groups. Also used to determine the bounds occupied by a group of controls. |
| `FLevelStack` | `TControlCreatorLevelStack` | `TControlCreatorLevelStack = TObjectList<TControlCreatorLevel>;` <br>Stack that stores level information. Calls to `.SubLevel` and `.SuperLevel` methods add and remove levels from the stack, respectively. The level at the top is the current level. <br><br>A level is an object of class `TControlCreatorLevel` that contains information such as `Direction`, `CurrentTop`, `CurrentLeft`, `VerticalSpace`, `HorizontalSpace`, `MaxControlHeight`, `MaxControlWidth`, among other properties used to control the positioning and size of controls as they are added through `.Add` methods.|

**Private Methods**

| Method | Return | Description |
|--------|--------|-------------|
| `GetControls` | `TControlList` | Returns list of controls added via `.Add` |
| `MoveTopLeftAfterControl(AControl: TControl)` | - | Moves `CurrentTop` and `CurrentLeft` of the current level after `AControl`. If `Direction` is `cpdVertical`, `Top` and `Left` will be to the right of the control; if `cpdHorizontal`, they will be below the control. <br>Position calculation also considers the `VerticalSpace` and `HorizontalSpace` properties of the current level and the `Align` property of `AControl`. |
| `MoveTopLeftAfterRect(const ARect: TRect; AAlign: TAlign)` | - | Same as previous, but considering `ARect`. <br>`AAlign` = `alTop` applies the change only to `Top`. <br>`AAlign` = `alLeft` applies the change only to `Left`. <br>`AAlign` = `alNone` applies the change only to `Top` and `Left`. <br>In `FMX`, the `AAlign` types are handled according to the framework. |
| `MoveTopLeftAfterBound(ABounds: TControlGroupBounds)` | - | Same as previous, but considering an object of type `TControlGroupBounds`. |
| `AddToGroups(AControl: TControl; const AGroups: array of string)` | - | Adds the control to one or more groups defined in `AGroups`. |
| `GetGroupBounds(const AGroupName: string)` | `TControlGroupBounds` | Returns a `record` of type `TControlGroupBounds` with dimension information of a control group. |
| `GetCurrentLevel` | `TControlCreatorLevel` | Returns the current level |
| `GetContentWidth` | `Single` | Returns the width occupied by all added controls. Uses `Single` type for better compatibility with the Firemonkey framework. |
| `GetContentHeight` | `Single` | Returns the height occupied by all added controls. Uses `Single` type for better compatibility with the Firemonkey framework. |
| `GetComponentRegistry` | `TComponentRegistry` | Returns the registry object of added components. This object is defined in the constructor call by the `ARegistryContextKey` parameter or by the `ARegistryContextHandle` parameter. Different Creators can share the same registry of their components. |
| `GetItem(const AName: string)` | `TControl` | Returns the Control identified by `AName`. |
| `ApplyDefaultControlSize(AControl: TControl)` | - | `TControlCreator` has optional fields `ControlHeight` and `ControlWidth` that can be used to set default values for any Control added via `.Add`. `ApplyDefaultControlSize` applies these default values to controls when they are added. |
| `CanAddToGrid` | `Boolean` | Validates if a new control to be created can be added to the Grid. Requires that `TControlCreator` is in Grid mode and that there is an available cell for inclusion. |

**Constructors**

| Constructor | Typical Usage |
|-------------|---------------|
| `Create(ARegistryContextKey: string = '')` | Instantiates a new `TControlCreator`. If `ARegistryContextKey` is provided, it will use the existing registry with the same name; otherwise, it will create a new one. If the key is not provided, a unique random key will be generated, resulting in a new registry. |
| `Create(ARegistryContextHandle: IRegistryContextHandle)` | Instantiates a `TControlCreator` reusing an existing component registry obtained via `ARegistryContextHandle`. |

**Public Methods**

| Method | Return | Description |
|--------|--------|-------------|
| `GetControl<T: TControl>(const AName: string)` | `T` | Generic method that returns a control by name. The control type must be specified. |
| `GetControl(const AName: string)` | `TControl` | Returns a Control by name. |
| `GetControlsBounds(AControlsNames: array of string)` | `TControlGroupBounds` | Returns a `TControlGroupBounds` object containing the bounds (Top, Left, Height and Width) of a group of controls identified by name. |
| `GetNamedControl(const AName: string)` | `TControl` | Returns a control by its name. |
| **Methods** Defined in the Helper Class |
| `SetupControlBuilderForGridMode<TBuild>(AControlBuilder: IControlBuilder<TBuild>)` | - | When a control is added via `.Add` in Grid mode, `AControlBuilder` is adjusted to represent the correct sizes and positions for the cell where it is being inserted. |
| `CreateControl<TBuild>(ABuilder: IControlBuilder<TBuild>; AOwner: TComponent = nil)` | `TBuild` | Instantiates the control added via `.Add`. Before calling the `.build` method of the `Builder` object, it makes the necessary adjustments for size and position definition according to mode and configurations. |
| `External(const AProc: TControlCreatorObjProc)` | `TControlCreator` | Allows injecting a procedure of type `TControlCreatorObjProc` that receives the `TControlCreator` object itself as a parameter.<br><br>`TControlCreatorObjProc = procedure(const ABuilder: TControlCreator) of object;` |
| `External(const AProc: TControlCreatorProc)` | `TControlCreator` | Allows injecting a procedure of type `TControlCreatorProc` that receives the `TControlCreator` object itself as a parameter.<br><br>`TControlCreatorProc = {$IFNDEF FPC}reference to{$ENDIF} procedure(ABuilder: TControlCreator);` |
| `WithOwnerAndParent(AOwner: TComponent; AParent: TWinControl)` ⚠️ *deprecated* | `TControlCreator` | Deprecated, use `.SetOwnerAndParent` instead. (Under FMX, `AParent` is a `TFmxObject` instead.) |
| `SetOwnerAndParent(AOwner: TComponent; AParent: TWinControl)` | `TControlCreator` | Fluent method that sets both the `Owner` and the initial `Parent` used for controls added afterwards — the usual first call in a chain. (Under FMX, `AParent` is a `TFmxObject` instead.) |
| `WithParent(AParent: TWinControl)` ⚠️ *deprecated* | `TControlCreator` | Deprecated, use `.SetParent` instead. |
| `SetParent(AParent: TWinControl)` | `TControlCreator` | Fluent method that sets the `Parent` used for controls added afterwards, without touching `Owner`. |
| `SetSpace(AVerticalSpace, AHorizontalSpace: Single)` | `TControlCreator` | Defines the horizontal and vertical spacing between controls added in the Current Level. |
| `SubLevel(AGroupName: string = '')` | `TControlCreator` | Starts a new level in the level stack without linking to a container. `FLevelStack.Add(CurrentLevel.Clone);`. If `AGroupName` is provided, the added controls will be linked to the Group passed as parameter. |
| `SubLevel(ADirection: TControlCreatorDirection; AGroupName: string = '')` | `TControlCreator` | Same as previous, but setting a new direction. |
| `SubLevel(AControlBuilder: TControlBuilder; AGroupName: string = '')` | `TControlCreator` | Same as previous, but defining a container, such as TPanel for example — the container is created and becomes the new level's `Parent`. If `AGroupName` is provided, links the added controls to this group. |
| `SubLevel<TBuild: class>(AControlBuilder: IControlBuilder<TBuild>; AGroupName: string = '')` | `TControlCreator` | Same as previous methods. Generic to allow different classes than `TControlBuilder` to be used as well, as long as they implement `IControlBuilder`. |
| `SubLevel(...)` — direction/builder combinations | `TControlCreator` | The four shapes above also combine with an explicit `ADirection: TControlCreatorDirection` parameter (e.g. `SubLevel(AControlBuilder, ADirection, AGroupName)`), for a total of 6 `SubLevel` overloads. |
| `SuperLevel` | `TControlCreator` | Returns to the previous level (or above). Raises if called with no level to return to. |
| `SiblingSubLevel(...)` — ~12 overloads | `TControlCreator` | Shorthand for `.SuperLevel` followed by `.SubLevel(...)` (optionally `.Break`-ing first) — closes the current level and immediately opens a *sibling* one, in one call. Exists in the same combinations as `SubLevel` (with/without a builder, with/without an explicit `ADirection`), plus an extra `ABreak: Boolean` parameter to control whether the parent level should break to a new line before the sibling is opened. This is the family responsible for the "overload explosion" mentioned below — prefer the `TControlCreatorSiblingSubLevelOptions` overload for anything beyond the simplest calls. |
| `SiblingSubLevel(const AOptions: TControlCreatorSiblingSubLevelOptions)` | `TControlCreator` | Same as above, but taking a single chainable options value instead of a dedicated overload per parameter combination — see [`TControlCreatorSiblingSubLevelOptions`](#tcontrolcreatorsiblingsubleveloptions) below. Also available combined with a builder: `SiblingSubLevel<TBuild>(AControlBuilder, AOptions)` / `SiblingSubLevel(AControlBuilder: TControlBuilder, AOptions)`. |
| `SiblingSubLevelWithBreak(...)` ⚠️ *deprecated* — 6 overloads | `TControlCreator` | Deprecated shorthand for `SiblingSubLevel(..., ABreak: True)` — i.e. it only ever existed to hard-code `ABreak` to `True`. Use `SiblingSubLevel(..., True)` or `SiblingSubLevel(TControlCreatorSiblingSubLevelOptions.Create.WithBreak)` instead. |
| `SetVerticalSpace(AVerticalSpace: Single)` | `TControlCreator` | Defines the vertical spacing between added controls. |
| `SetHorizontalSpace(AHorizontalSpace: Single)` | `TControlCreator` | Defines the horizontal spacing between added controls. |
| `SetTop(ATop: Single)` | `TControlCreator` | Defines the Top - in CurrentLevel - of the next control to be added. Also defines `InitialTop` which is used to define the new Top position when calling `.Break`, `.BreakLine` or `.BreakColumn`. |
| `SetLeft(ALeft: Single)` | `TControlCreator` | Defines the Left - in CurrentLevel - of the next control to be added. Also defines `InitialLeft` which is used to define the new Left position when calling `.Break`, `.BreakLine` or `.BreakColumn`. |
| `SetTopLeft(ATop, ALeft: Single)` | `TControlCreator` | Defines the Top and Left - in CurrentLevel - of the next control to be added. Also defines `InitialTop` and `InitialLeft` which are used to define the new Top and Left position when calling `.Break`, `.BreakLine` or `.BreakColumn`. |
| `SetTopLeftNearControl(AControlName: string; APosition: TRelativePosition)` | `TControlCreator` | Defines Top and Left based on an existing control. APosition defines whether below or to the right of the control. Takes into account the spacing defined in Vertical and Horizontal Spaces. |
| `SetTopLeftNearControls(AControlsNames: string; APosition: TRelativePosition)` | `TControlCreator` | Same as previous, but considering the combination of controls passed as parameters in `AControlsNames`. |
| `SetTopLeftNearGroup(const AGroupName: string; APosition: TRelativePosition)` | `TControlCreator` | Same as previous but considering a group of controls. |
| `IncTop(AIncTop: Single)` | `TControlCreator` | Increments Top without changing `InitialTop`. |
| `IncLeft(AIncLeft: Single)` | `TControlCreator` | Increments Left without changing `InitialLeft`. |
| `IncTopLeft(AIncTop, AIncLeft: Single)` | `TControlCreator` | Increments Top and Left without changing `InitialTop` and `InitialLeft`. |
| `SetDirection(ADirection: TControlCreatorDirection)` | `TControlCreator` | Defines the direction in which controls will be inserted. Vertically or Horizontally. <br><br>Each time a control is added, the Top and Left position (`CurrentTop` and `CurrentLeft`) is calculated to be used the next time another Control is added. For this calculation, the current direction is taken into account.<br><br>`TControlCreatorDirection = (cpdHorizontal, cpdVertical);` |
| `SetControlHeight(AHeight: Single)` | `TControlCreator` | Defines a default height value for all controls that will be added via `.Add` if no value is defined for them. |
| `SetControlWidth(AWidth: Single)` | `TControlCreator` | Defines a default width value for all controls that will be added via `.Add` if no value is defined for them. |
| `SetControlWidthAndHeight(AWidth, AHeight: Single)` | `TControlCreator` | Defines a default height and width value for all controls that will be added via `.Add` if these values are not defined for them. |
| `UnsetControlHeight` | `TControlCreator` | Removes the default value definition for height of controls added via `.Add`. |
| `UnsetControlWidth` | `TControlCreator` | Removes the default value definition for width of controls added via `.Add`. |
| `UnsetControlWidthAndHeight` | `TControlCreator` | Removes the default value definition for height and width of controls added via `.Add`. |
| `GridInit(ARows, ACols: Integer)` | `TControlCreator` | Starts Grid mode by specifying the number of rows and columns. Internally calls `.SubLevel`. |
| `GridFinish` | `TControlCreator` | Exits Grid mode. Internally calls `.SuperLevel`. |
| `GridCellSpan(ACellSpan: Integer)` | `TControlCreator` | Expands the size of the current cell to occupy more than one cell. Takes into account the current direction defined in `.SetDirection`. |
| `GridRowSpan(ARowSpan: Integer)` | `TControlCreator` | Expands the size of the current cell to occupy more than one row. |
| `GridColSpan(AColSpan: Integer)` | `TControlCreator` | Expands the size of the current cell to occupy more than one column. |
| `GridSetCellWidthAndHeight(AWidth, AHeight: Integer)` | `TControlCreator` | Defines the default size of all grid cells. |
| `GridSetColWidth(ACol: Integer; AWidth: Single)` | `TControlCreator` | Defines a custom width for a specific Grid column. The other columns will remain with the default width value. |
| `GridSetRowHeight(ARow: Integer; AHeight: Single)` | `TControlCreator` | Defines a custom height for a specific Grid row. The other rows will remain with the default height value. |
| `GridSetColOffset(ACol: Integer; AOffset: Single)` | `TControlCreator` | Defines a vertical offset for a specific column. |
| `GridSetRowOffset(ARow: Integer; AOffset: Single)` | `TControlCreator` | Defines a horizontal offset for a specific row. |
| `GridSkipCell` | `TControlCreator` | Skips the current cell. |
| `GridSkipCells(ANumCells: Integer)` | `TControlCreator` | Skips the next `ANumCells` cells. |
| `GridGotoCell(ARow, ACol: Integer)` | `TControlCreator` | Moves the current cell to a new position. |
| `GridAutoExpand` | `TControlCreator` | Defines that the grid can automatically expand rows and columns. |
| `GridAutoExpandRows` | `TControlCreator` | Defines that the grid can automatically expand rows. |
| `GridAutoExpandCols` | `TControlCreator` | Defines that the grid can automatically expand columns. |
| `GridCellPosition` | `TControlCreator` | Defines the position where controls will be inserted in the cell. <br><br>`TCellPosition = (cpCenter, cpTop, cpRight, cpBottom, cpLeft, cpTopRight, cpTopLeft, cpBottomRight, cpBottomLeft);` |
| `GridCellNoStrech` | `TControlCreator` | Defines that the cell will not stretch the control to fit the cell. |
| `GridCellStrechAll` | `TControlCreator` | Defines that the cell will stretch the control vertically and horizontally to fit the cell. |
| `GridCellStrechHorizontal` | `TControlCreator` | Defines that the cell will stretch the control horizontally to fit the cell. |
| `GridCellStrechVertical` | `TControlCreator` | Defines that the cell will stretch the control vertically to fit the cell. |
| `GridReturnNumberOfRows` | `TControlCreator` | Returns the current number of rows through `ARows`. |
| `GridReturnNumberOfCols` | `TControlCreator` | Returns the current number of columns through `ACols`. |
| `BreakLine` | `TControlCreator` | Moves the Current Top position to a line below while Current Left returns to the Initial Left. |
| `BreakColumn` | `TControlCreator` | Moves the Current Left position to a column to the right while Current Top returns to the Initial Top. |
| `BreakLine(AIncTop: Single)` | `TControlCreator` | Same as `.BreakLine` but incrementing Top. |
| `BreakColumn(AIncLeft: Single)` | `TControlCreator` | Same as `.BreakColumn` but incrementing Left. |
| `Break` | `TControlCreator` | Calls `.BreakLine` or `.BreakColumn` according to the current direction. |
| `Break(AIncTopOrLeft: Single)` | `TControlCreator` | Same as `.Break` but incrementing Top or Left. |
| `Add(AControlBuilder: TControlBuilder)` | `TControlCreator` | Adds a new control through a `TControlBuilder` object. |
| `Add<TBuild: class>(AControlBuilder: IControlBuilder<TBuild>)` | `TControlCreator` | Generic overload of the above — accepts any builder implementing `IControlBuilder<TBuild>`, not just `TControlBuilder`. Also available with an extra `const AGroups: array of string` parameter to add the control to one or more groups immediately. |
| `Add(AClass: TControlClass; ...)` — 6 overloads | `TControlCreator` | Shorthand that builds a plain `TControlBuilder` internally, so you don't need to write `Add(TControlBuilder.Create(...))` for the common case. Combines an optional `const AName: string`, an optional `out Reference`, and an optional `AProc: TControlSetupProcObj` setup callback, e.g. `Add(TPanel, 'MyPanel', MyPanelVar)` or `Add(TPanel, 'MyPanel', proc(C: TControl) begin ... end)`. |
| `MoveControls(const AControl: TControl; const ADX, ADY: Single)` | `TControlCreator` | Moves a control vertically and/or horizontally. |
| `MoveControls(const AControlNames: array of string; const ADX, ADY: Single)` | `TControlCreator` | Moves the controls identified by the names in `AControlNames`. |
| `AlignControlsRight(const AControlNames, AReferenceGroup: array of string; const ARightPadding: Single = 0)` | `TControlCreator` | Aligns a set of Controls to the right, considering a group of other controls as reference. If `ARightPadding` is provided, decrements the value in positioning. |
| `CenterControlsHorizontally(const AControlNames, AReferenceGroup: array of string)` | `TControlCreator` | Centers horizontally a set of controls, considering a group of other controls as reference. |
| `CenterControlsVertically(const AControlNames, AReferenceGroup: array of string)` | `TControlCreator` | Centers vertically a set of controls, considering a group of other controls as reference. |
| `CenterControlsInParentVertically(const AControlNames: array of string)` | `TControlCreator` | Centers vertically a set of controls, considering the dimensions of the controls' Parent. |
| `CenterControlsInParentHorizontally(const AControlNames: array of string)` | `TControlCreator` | Centers horizontally a set of controls, considering the Parent of the controls as reference. |
| `CenterControlInParentHorizontally` | `TControlCreator` | Centers horizontally the last added control, considering the Parent of the control as reference. |
| `RecalcParentHeight(AExtraHeight: Single = 0)` | `TControlCreator` | Recalculates the height of the Parent (container) of the current level based on the dimension of already added controls. If `AExtraHeight` is provided, increments the height by the value. |
| `RecalcParentWidth(AExtraWidth: Single = 0)` | `TControlCreator` | Recalculates the width of the Parent (container) of the current level based on the dimension of already added controls. If `AExtraWidth` is provided, increments the width by the value. |
| `RecalcParentSize(AExtraHeight: Single = 0; AExtraWidth: Single = 0)` | `TControlCreator` | Recalculates the width and height of the Parent (container) of the current level based on the dimension of already added controls. If `AExtraWidth` and `AExtraHeight` are provided, increments the width and height by the values. |
| `CopyHeight(const AControlNames, AReferenceGroup: array of string)` | `TControlCreator` | Copies the height formed by the Bounds of the control group passed in `AReferenceGroup` to each of the controls defined in `AControlNames`. |
| `CopyWidth(const AControlNames, AReferenceGroup: array of string)` | `TControlCreator` | Copies the width formed by the Bounds of the control group passed in `AReferenceGroup` to each of the controls defined in `AControlNames`. |
| `CopySize(const AControlNames, AReferenceGroup: array of string)` | `TControlCreator` | Copies the width and height formed by the Bounds of the control group passed in `AReferenceGroup` to each of the controls defined in `AControlNames`. |
| `ReturnCurrentLevel(var ACurrentLevel: TControlCreatorLevel)` | `TControlCreator` | Returns the Current Level through the `ACurrentLevel` parameter. |
| `ReturnLastControl(out AControl: TControl)` | `TControlCreator` | Returns the last control added via `.Add`. |
| `DiscardReferences` *(on the builder, not the creator)* | | Not a `TControlCreator` method — see [`TObjectBuilderBase`](#tobjectbuilderbase). Mentioned here because it's what makes `out Reference` variables come back `nil` (instead of keeping stale/garbage data) when a control never actually gets built — e.g. `Add` bailing out because the grid is already full. |

##### `TControlCreatorSiblingSubLevelOptions`

A small chainable record used by the `SiblingSubLevel(const AOptions: ...)` overloads (see table above) to avoid needing a dedicated positional overload for every combination of `Direction`/`GroupName`/`Break`. It doesn't replace the positional overloads — they still exist and still work — it's just an additional way to call `SiblingSubLevel` that doesn't grow combinatorially if more optional parameters are ever added.

| Method | Return | Description |
|--------|--------|-------------|
| `Create` *(class function)* | `TControlCreatorSiblingSubLevelOptions` | Starts a new, empty options value (no direction override, no group, no break). |
| `WithDirection(ADirection: TControlCreatorDirection)` | `TControlCreatorSiblingSubLevelOptions` | Sets the direction to switch the new sibling level to. If never called, the sibling level inherits whatever direction the parent level already had. |
| `WithGroup(const AGroupName: string)` | `TControlCreatorSiblingSubLevelOptions` | Sets the group name for the new sibling level, same meaning as the `AGroupName` parameter on the positional overloads. |
| `WithBreak(ABreak: Boolean = True)` | `TControlCreatorSiblingSubLevelOptions` | Sets whether the parent level should `.Break` before the sibling level opens. |

**Example**
```pascal
Creator.SiblingSubLevel(
  TControlCreatorSiblingSubLevelOptions.Create
    .WithDirection(cpdVertical)
    .WithGroup('second')
    .WithBreak
);
```

**Properties**

| Property | Type | Function |
|----------|------|----------|
| `Registry` | `TComponentRegistry` | Returns the `TComponentRegistry` used to store the created components. |
| `Items[const AName: string]` | `TControl` | Returns a component from the registry by its name. |
| `NamedControls[const AName: string]` | `TControl` | Returns a control by its name. |
| `ContentWidth` | `Single` | Returns the width occupied by the added components. |
| `ContentHeight` | `Single` | Returns the height occupied by the added components. |
| `CurrentLevel` | `TControlCreatorLevel` | Returns the current `TControlCreatorLevel` object. |
| `Controls` | `TControlList` | Returns list of added controls. If the `Registry` is shared, it will return objects added from other Creators. |

---

#### TMenuCreator

`TMenuCreator` centralizes the creation of menu trees (`TMainMenu`, `TPopupMenu`, and their items) using the same fluent, level-based style as `TControlCreator`, without needing to drag menu components onto a form at design time.

Unlike `TControlCreator`, `TMenuCreator` has no grid mode and no positioning logic (`SetTop`/`SetLeft`/`Break`/etc.) — menus have no visual layout to compute, so its level stack only tracks which `TMenuItem` (or `TFmxObject`, under FMX) new items should be parented to.

**Private Fields**

| Field | Type | Description |
|-------|------|-------------|
| `FOwner` | `TComponent` | Defines the *Owner* of menus/items added through `.AddMenu`/`.AddMenuItem`. |
| `FRegistryContextHandle` | `IRegistryContextHandle` | Indicates the *ContextHandle* used to register the created components. Allows sharing the registry with other *Creators*, such as `TControlCreator`, `TComponentCreator`, etc. |
| `FLevelStack` | `TMenuCreatorLevelStack` | `TMenuCreatorLevelStack = TObjectList<TMenuCreatorLevel>;` <br> Stack of `TMenuCreatorLevel` objects (each holding just a `Parent: TMenuItem`, or `TFmxObject` under FMX). Calls to `.SubLevel` and `.SuperLevel` push and pop levels, respectively — the top of the stack is the current level, i.e. which item newly-added menu items become children of. |

**Constructors**

| Constructor | Typical Usage |
|-------------|---------------|
| `Create(ARegistryContextKey: string = '')` | Instantiates a new `TMenuCreator`. If `ARegistryContextKey` is provided, it will reuse the existing registry with that key; otherwise a new one is created (with a random key if none is given). |
| `Create(ARegistryContextHandle: IRegistryContextHandle)` | Instantiates a `TMenuCreator` reusing an existing component registry obtained via `ARegistryContextHandle`. |

**Public Methods**

| Method | Return | Description |
|--------|--------|-------------|
| `WithOwner(AOwner: TComponent)` ⚠️ *deprecated* | `TMenuCreator` | Deprecated, use `.SetOwner` instead. |
| `SetOwner(AOwner: TComponent)` | `TMenuCreator` | Fluent method that sets the owner used for menus/items added afterwards. |
| `External(const AProc: TMenuCreatorObjProc)` | `TMenuCreator` | Injects a procedure of type `TMenuCreatorObjProc` (`procedure(const ABuilder: TMenuCreator) of object`) that receives the creator itself, useful for building menu items in a loop/batch from a method. |
| `External(const AProc: TMenuCreatorProc)` | `TMenuCreator` | Same as above, but for a plain (non-`of object`) procedure of type `TMenuCreatorProc`. |
| `AddMenu(AMenuBuilder: TMenuBuilder)` | `TMenuCreator` | Creates a top-level menu (e.g. a `TMainMenu` or `TPopupMenu`) from `AMenuBuilder` and makes it the current level's root — subsequent `.AddMenuItem`/`.SubLevel` calls attach items to it. |
| `AddMenuItem(AMenuItemBuilder: TMenuItemBuilder)` | `TMenuCreator` | Adds a new `TMenuItem` (built from `AMenuItemBuilder`) as a child of the current level's parent. Requires an `.AddMenu` (or `.SubLevel`) to have run first — raises a clear exception otherwise, since the root level starts with no parent to attach to. |
| `SubLevel(AMenuItemBuilder: TMenuItemBuilder)` | `TMenuCreator` | Adds a new `TMenuItem` like `.AddMenuItem`, but also pushes a new level using that item as the parent — so subsequent `.AddMenuItem` calls add *sub*-items to it (builds a submenu). |
| `SuperLevel` | `TMenuCreator` | Pops the current level, returning to the parent one pushed by the last `.SubLevel` (or to the menu root). Raises if called at the root level, since there is nothing to return to. |
| `GetMenu<T: TMenu>(const AName: string): T` | `T` | Generic method that returns a previously added menu by name, already cast to `T`. |
| `GetMenu(const AName: string): TMenu` | `TMenu` | Same as above, without the generic cast. |
| `GetMenuItem<T: TMenuItem>(const AName: string): T` | `T` | Generic method that returns a previously added menu item by name, already cast to `T`. |
| `GetMenuItem(const AName: string): TMenuItem` | `TMenuItem` | Same as above, without the generic cast. |

**Properties**

| Property | Type | Description |
|----------|------|-------------|
| `CurrentLevel` | `TMenuCreatorLevel` | Returns the current level object (just its `Parent` item/object matters here). |
| `Registry` | `TComponentRegistry` | Returns the `TComponentRegistry` used to store the created menus/items — see [TComponentRegistry](#tcomponentregistry). |

**Example**
```pascal
var
  Creator: TMenuCreator;
begin
  Creator := TMenuCreator.Create;
  try
    Creator
      .SetOwner(Self)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
        .SubLevel(TMenuItemBuilder.Create(TMenuItem, 'FileMenu').WithCaption('&File'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileNew').WithCaption('&New'))
          .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'FileOpen').WithCaption('&Open'))
        .SuperLevel
        .AddMenuItem(TMenuItemBuilder.Create(TMenuItem, 'EditMenu').WithCaption('&Edit'))
    ;

    Self.Menu := Creator.GetMenu<TMainMenu>('MainMenu');
  finally
    Creator.Free;
  end;
end;
```

---

#### TOPCBCreators

A thin convenience wrapper around the three creators (`TComponentCreator`, `TControlCreator`, `TMenuCreator`), useful when a screen needs more than one of them sharing the **same** component registry — instead of creating and wiring each one manually with a matching `ARegistryContextKey`/`IRegistryContextHandle`, `TOPCBCreators` creates all three sharing one automatically.

Inheritance

    TOPCBCreators = class

**Private Fields**

| Field | Type | Description |
|-------|------|-------------|
| `FRegistryContextHandle` | `IRegistryContextHandle` | The single registry context shared by all three creators below. |
| `FComponentCreator` | `TComponentCreator` | Lazily-created internal `TComponentCreator` instance. |
| `FControlCreator` | `TControlCreator` | Lazily-created internal `TControlCreator` instance. |
| `FMenuCreator` | `TMenuCreator` | Lazily-created internal `TMenuCreator` instance. |

**Constructors**

| Constructor | Description |
|-------------|-------------|
| `Create(const ARegistryContextKey: string = '')` | Creates the wrapper. If `ARegistryContextKey` is given, all three internal creators share the registry for that key; otherwise a fresh shared registry is used. |

**Public Methods**

| Method | Return | Description |
|--------|--------|-------------|
| `AsComponentCreator` | `TComponentCreator` | Returns the internal `TComponentCreator`, sharing the registry with the other two. |
| `AsControlCreator` | `TControlCreator` | Returns the internal `TControlCreator`, sharing the registry with the other two. |
| `AsMenuCreator` | `TMenuCreator` | Returns the internal `TMenuCreator`, sharing the registry with the other two. |

**Example**
```pascal
var
  Creators: TOPCBCreators;
begin
  Creators := TOPCBCreators.Create;
  try
    Creators.AsControlCreator
      .SetOwnerAndParent(Self, Self)
      .Add(TControlBuilder.Create(TPanel, 'MainPanel'))
    ;

    Creators.AsMenuCreator
      .SetOwner(Self)
      .AddMenu(TMenuBuilder.Create(TMainMenu, 'MainMenu'))
    ;

    // Both creators above share the same registry, so either one can look
    // up components/controls the other created:
    Creators.AsComponentCreator.GetComponent('MainMenu');
  finally
    Creators.Free;
  end;
end;
```

---

### Registry

#### TComponentRegistry

`TComponentRegistry` is the internal, name-keyed registry that backs every creator (`TComponentCreator`, `TControlCreator`, `TMenuCreator`) — it is what actually stores "the component named X" and answers `GetComponent`/`GetControl`/`Items[]` lookups. Most code never touches it directly: a creator's constructor (`ARegistryContextKey`/`ARegistryContextHandle` parameter) already creates or reuses one internally, and creators expose it through their `.Registry` property when direct access is useful (e.g. to look something up that a *different* creator added, when both share a registry).

**Context keys and sharing**

Each registry instance lives behind a string **context key**. Two creators constructed with the same key (or the same `IRegistryContextHandle`) share the *same* `TComponentRegistry` instance — this is how `TOPCBCreators` (see above) makes its three creators see each other's components. The registry is reference-counted internally per key: it stays alive as long as at least one creator/handle for that key exists, and is released automatically once the last one is freed.

You normally never call the registry's own context-management methods (`ForContext`/`ReleaseContext`) — they are `protected`, meant for the creators (and `TRegistryContextHandle`) to call internally. If you need a registry handle directly (for example, to share a registry across creators created at different points in your code, without threading an `ARegistryContextKey` string through everything), use the public class method below:

| Class Method | Return | Description |
|--------------|--------|-------------|
| `GetContextHandle(AKey: string): IRegistryContextHandle` | `IRegistryContextHandle` | Returns a reference-counted handle to the registry for `AKey` (creating it if needed). Pass the same handle to multiple creators' `Create(ARegistryContextHandle)` constructor to make them share the registry; releasing the interface reference (or calling `.ReleaseContext`) releases the registry's hold. |
| `ClearAll` | | Frees **every** registry currently alive in the process, regardless of context key. Meant for wholesale teardown (e.g. application shutdown/tests), not routine use — it does not check whether some other part of the code still holds a live reference to one of those registries. |
| `GetComponentFromContext<T: TComponent>(const AContextKey, AComponentlName: string): T` | `T` | Convenience one-shot lookup: gets (or creates) the registry for `AContextKey`, looks up `AComponentlName`, and releases the context — without you needing to hold a handle at all. Generic overload, cast to `T`. |
| `GetComponentFromContext(const AContextKey, AComponentlName: string): TComponent` | `TComponent` | Same as above, without the generic cast. |
| `GetControlFromContext<T: TControl>(const AContextKey, AControlName: string): T` | `T` | Same idea as `GetComponentFromContext`, but for controls. Generic overload, cast to `T`. |
| `GetControlFromContext(const AContextKey, AControlName: string): TControl` | `TControl` | Same as above, without the generic cast. |

**Instance API**

Once you have a `TComponentRegistry` instance (typically via a creator's `.Registry` property), this is what you can do with it:

| Method | Return | Description |
|--------|--------|-------------|
| `Add(AControl: TControl; const AName: string = '')` | | Registers a control under `AName` (auto-generating a unique name if omitted) and starts tracking its destruction (see `RegisterComponentForNotification`). Used internally by `TControlCreator.Add`. |
| `AddComponent(AComponent: TComponent; const AName: string = '')` | | Same as `Add`, but for any `TComponent` (visual or not) — used internally by `TComponentCreator.Add`. |
| `GetComponent<T: TComponent>(const AName: string): T` | `T` | Generic lookup by name, cast to `T`; raises if not found. |
| `GetComponent(const AName: string): TComponent` | `TComponent` | Same as above, without the generic cast; raises if not found. |
| `TryGetComponent<T: TComponent>(const AName: string; out AComponent: T): Boolean` | `Boolean` | Non-raising lookup: returns `False` (and leaves `AComponent` unset) instead of raising when `AName` doesn't exist. |
| `TryGetComponent(const AName: string; out AComponent: TComponent): Boolean` | `Boolean` | Same as above, without the generic cast. |
| `GetControl<T: TControl>(const AName: string): T` | `T` | Generic lookup by name, restricted to visual controls, cast to `T`; raises if not found. |
| `GetControl(const AName: string): TControl` | `TControl` | Same as above, without the generic cast; raises if not found. |
| `TryGetControl<T: TControl>(const AName: string; out AControl: T): Boolean` | `Boolean` | Non-raising control lookup. |
| `TryGetControl(const AName: string; out AControl: TControl): Boolean` | `Boolean` | Same as above, without the generic cast. |
| `RegisterComponentForNotification(AComp: TComponent)` | | Hooks the component's `FreeNotification` so the registry removes its entry automatically when the component is destroyed — called internally by `Add`/`AddComponent`, rarely needed directly. |
| `UnregisterComponentForNotification(AComp: TComponent)` | | Undoes the above; also handled automatically in the normal flow. |

**Properties**

| Property | Type | Description |
|----------|------|-------------|
| `Components` | `TComponentList` | List of every registered component (visual or not). |
| `Controls` | `TControlList` | List of every registered *visual* control (subset of `Components`). |
| `NamedComponents` | `TStrComponentDictionary` | The raw name→component dictionary backing the registry. |
| `Items[ACompName: string]` | `TComponent` | Default indexed property — `Registry['Foo']` looks up by name across *all* registered components (visual or not), unlike `TControlCreator.NamedControls` which only searches controls. |
| `RegistryLifetime` | `TRegistryLifetime` | `(rlTransient, rlPersistent)`. Governs when the registry can be released; `rlPersistent` keeps it alive even if the reference count would otherwise drop it. |
| `ContextKey` | `string` | The context key this registry instance is registered under. |

---

This reference covers every public class the library exposes. For runnable, end-to-end code, see the "Examples Gallery" section of the [main README](../README.md), or browse [`examples/Builders`](../examples/Builders) directly.