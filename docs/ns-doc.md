# (ns `aod.c.midi`)

## `note-number` <small>procedure?</small>
(note-number status data1 data2) Returns either the note or -1
## `note-off?` <small>procedure?</small>
(note-off? status data1 data2)
## `note-on?` <small>procedure?</small>
(note-on? status data1 data2)
# (ns `aod.c.curl`)
Basic bindings for libcurl
## `easy-escape` <small>procedure?</small>
(easy-escape string)
## `curl` <small>procedure?</small>
(curl url (out #f) (opts *default-curl-opts*))
## `*default-opts*` <small>let?</small>
value `(inlet :ssl-verify-peer 1 :follow-location 1)`
# (ns `aod.c.json`)
Basic bindings for nlohmann/json
## `parse` <small>procedure?</small>
(parse json-str) Returns a json c-object
## `type-json` <small>integer?</small>

# (ns `aod.imgui.macros`)
Some macros to make life easier while working with ImGui.
The usual syntax is (args . body)
- args are applied to corresponding raw imgui function
- body is executed either in a when block (eg when a menu items is active)
  or wrapped between the begin/end calls
## `horizontal` <small>macro?</small>
(horizontal . body)
executes first element of body and then inserts any next element with the same-line called before
## `menu-item` <small>macro?</small>

## `menu` <small>macro?</small>

## `menu-bar` <small>macro?</small>

## `main-menu-bar` <small>macro?</small>

## `group` <small>macro?</small>

## `child` <small>macro?</small>

## `maximized` <small>macro?</small>

## `window` <small>macro?</small>
(window args . body)
applies args to imgui/begin, executes body and calls imgui/end
# (ns `aod.imgui.helpers`)

## `frgb->u32` <small>procedure?</small>

## `draw-lines` <small>procedure?</small>

## `draw-arc` <small>procedure?</small>

## `draw-circle` <small>procedure?</small>

# (ns `aod.c.imgui.window-flags`)
One-to-one relation between ImGuiWindowFlags_* int values.
To use perform bitwise-or and pass the imgui begin as window flags
## `NoInputs` <small>integer?</small>

## `NoDecoration` <small>integer?</small>

## `NoNav` <small>integer?</small>

## `UnsavedDocument` <small>integer?</small>

## `NoNavFocus` <small>integer?</small>

## `NoNavInputs` <small>integer?</small>

## `AlwaysUseWindowPadding` <small>integer?</small>

## `AlwaysHorizontalScrollbar` <small>integer?</small>

## `AlwaysVerticalScrollbar` <small>integer?</small>

## `NoBringToFrontOnFocus` <small>integer?</small>

## `NoFocusOnAppearing` <small>integer?</small>

## `HorizontalScrollbar` <small>integer?</small>

## `MenuBar` <small>integer?</small>

## `NoMouseInputs` <small>integer?</small>

## `NoSavedSettings` <small>integer?</small>

## `NoBackground` <small>integer?</small>

## `AlwaysAutoResize` <small>integer?</small>

## `NoCollapse` <small>integer?</small>

## `NoScrollWithMouse` <small>integer?</small>

## `NoScrollbar` <small>integer?</small>

## `NoMove` <small>integer?</small>

## `NoResize` <small>integer?</small>

## `NoTitleBar` <small>integer?</small>

## `None` <small>integer?</small>

# (ns `aod.c.foreign`)
Provides a way to create heap allocated primitives like int* float*, int* array, char* array etc. For example, to create a c string call `(new-char[] size)`
## `new-char[]` <small>procedure?</small>
creates a heap allocated char[] (c-object)
## `type-char[]` <small>integer?</small>

## `new-float[]` <small>procedure?</small>
creates a heap allocated float[] (c-object)
## `type-float[]` <small>integer?</small>

## `new-int[]` <small>procedure?</small>
creates a heap allocated int[] (c-object)
## `type-int[]` <small>integer?</small>

## `new-bool[]` <small>procedure?</small>
creates a heap allocated bool[] (c-object)
## `type-bool[]` <small>integer?</small>

## `new-float` <small>procedure?</small>
creates a heap allocated float (c-object)
## `type-float` <small>integer?</small>

## `new-int` <small>procedure?</small>
creates a heap allocated int (c-object)
## `type-int` <small>integer?</small>

## `new-bool` <small>procedure?</small>
creates a heap allocated bool (c-object)
## `type-bool` <small>integer?</small>

# (ns `aod.c.imgui`)
ImGui bindings. The majority of the bindings are a one-to-one relationship with the underlying ImGui::foo calls
## `set-keyboard-focus-here` <small>procedure?</small>
SetKeyboardFocusHere (&optional offset)
focus keyboard on the next widget. Use positive 'offset' to access sub components of a multiple component widget. Use -1 to access previous widget
## `is-item-focused` <small>procedure?</small>
IsItemFocused
## `set-item-default-focus` <small>procedure?</small>
SetItemDefaultFocus
## `is-item-deactivated` <small>procedure?</small>
IsItemDeactivated
## `is-item-deactivated-after-edit` <small>procedure?</small>
IsItemDeactivatedAfterEdit
## `combo` <small>procedure?</small>
(combo name p-index labels)
- `p-index`: an `int*` pointer as returned from `aod.c.foreign/new-int`
- `labels`: a list of strings
## `input-text-multiline` <small>procedure?</small>
(input-text-multiline label p-buffer buffer-size)
`p-buffer`: a `char*` as returned from `aod.c.foreign/new-char[]`
## `input-text` <small>procedure?</small>
(input-text label *buffer buffer-size) *buffer is c-pointer to *char from aod.c.foreign/new-char[]
## `slider-int` <small>procedure?</small>
(slider-int label p-value min max)
`p-value`: `int*` pointer from `aod.c.foreign/new-int`
## `slider-float` <small>procedure?</small>
(slider-float label *value min max &optional (format "%.3f"))
## `color-edit-3` <small>procedure?</small>
(color-edit-3 label *values) *values: aod.c.foreign float[] array
## `set-color` <small>procedure?</small>
(set-color color-index color-u32)
## `color32` <small>procedure?</small>
(color32 r g b &optional alpha) input ranging from 0 to 255
Returns a u32 representation of the color 0xRRGGBBAA
## `draw-text` <small>procedure?</small>
(x y text color)
## `draw-line` <small>procedure?</small>
(x1 y1 x2 y2 col &optional thickness)
## `draw-circle-filled` <small>procedure?</small>
(cx cy r col &optional segments)
## `draw-arc` <small>procedure?</small>
(cx cy r a-min a-max col &optional segments thickness)
## `draw-circle` <small>procedure?</small>
(draw-circle cx cy r col &optional segments thickness)
## `dummy` <small>procedure?</small>
(dummy width height)
## `end-group` <small>procedure?</small>
(end-group)
## `begin-group` <small>procedure?</small>
(begin-group)
## `end-child` <small>procedure?</small>
(end-child)
## `begin-child` <small>procedure?</small>
(begin-child id) (string?)
## `same-line` <small>procedure?</small>
(same-line) puts the next element in the same line as the previously drawn element
## `menu-item` <small>procedure?</small>
(menu-item label) TODO add more arguments (&optional shortcut selected)
## `separator` <small>procedure?</small>
(separator)
## `end-menu` <small>procedure?</small>
(end-menu)
## `begin-menu` <small>procedure?</small>
(begin-menu label) label could be "File" for example
## `end-main-menu-bar` <small>procedure?</small>
(end-main-menu-bar)
## `begin-main-menu-bar` <small>procedure?</small>
(begin-main-menu-bar)
## `end-menu-bar` <small>procedure?</small>
(end-menu-bar)
## `begin-menu-bar` <small>procedure?</small>
(begin-menu-bar)
## `checkbox` <small>procedure?</small>
(checkbox label *value) *value is *bool pointer
## `small-button` <small>procedure?</small>
(small-button text)
## `button` <small>procedure?</small>
Button
## `align-text-to-frame-padding` <small>procedure?</small>
(align-text-to-frame-padding)
## `label` <small>procedure?</small>
(label ...) NOT DONE
## `text` <small>procedure?</small>
(text text) displays a text. the argument is just a scheme string
## `spacing` <small>procedure?</small>
(spacing)
## `end` <small>procedure?</small>
(end)
## `begin-maximized` <small>procedure?</small>
(begin-maximized title &optional window-flags) NOT PART OF IMGUI: A convenient way to do a maximized window
window-flags is just one int with bit flags set. There are already plenty set like NoTitleBar, NoResize etc.
## `begin` <small>procedure?</small>
(begin name &optional p-open window-flags)
- `name`: the name of the window, a scheme string
- `p-open`: a `bool*`, as returned from `aod.c.foreign/new-bool`. Closing the window modifies the pointer value
# (ns `aod.c.gl`)

## `save-screenshot` <small>procedure?</small>
(save-screenshot filename) Saves a screenshot of the current gl context
# (ns `aod.c.nfd`)
Some [nativefiledialog](https://github.com/mlabbe/nativefiledialog) bindings 
## `save` <small>procedure?</small>
(save) Save file dialog. Returns either the selected target filename or #f
## `open` <small>procedure?</small>
(open) Open file dialog. Returns either the selected filename or #f
