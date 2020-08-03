((aod.imgui.macros "Some macros to make life easier while working with ImGui.
The usual syntax is (args . body)
- args are applied to corresponding raw imgui function
- body is executed either in a when block (eg when a menu items is active)
  or wrapped between the begin/end calls" (window . "(window args . body)
applies args to imgui/begin, executes body and calls imgui/end") (maximized . "") (child . "") (group . "") (main-menu-bar . "") (menu-bar . "") (menu . "") (menu-item . "") (horizontal . "(horizontal . body)
executes first element of body and then inserts any next element with the same-line called before")) (aod.imgui.helpers "" (draw-circle . "") (draw-arc . "") (draw-lines . "") (frgb->u32 . "")) (aod.c.imgui "ImGui bindings. The majority of the bindings are a one-to-one relationship with the underlying ImGui::foo calls" (begin . "(begin name &optional *bool window-flags)
- name: the name of the window, a scheme string
- *bool: a pointer to bool, from aod.c.foreign. Closing the window modifies the pointer value") (begin-maximized . "(begin-maximized title &optional window-flags) NOT PART OF IMGUI: A convenient way to do a maximized window
window-flags is just one int with bit flags set. There are already plenty set like NoTitleBar, NoResize etc.") (end . "(end)") (spacing . "(spacing)") (text . "(text text) displays a text. the argument is just a scheme string") (label . "(label ...) NOT DONE") (align-text-to-frame-padding . "(align-text-to-frame-padding)") (button . "Button") (small-button . "(small-button text)") (checkbox . "(checkbox label *value) *value is *bool pointer") (begin-menu-bar . "(begin-menu-bar)") (end-menu-bar . "(end-menu-bar)") (begin-main-menu-bar . "(begin-main-menu-bar)") (end-main-menu-bar . "(end-main-menu-bar)") (begin-menu . "(begin-menu label) label could be \"File\" for example") (end-menu . "(end-menu)") (separator . "(separator)") (menu-item . "(menu-item label) TODO add more arguments (&optional shortcut selected)") (same-line . "(same-line) puts the next element in the same line as the previously drawn element") (begin-child . "(begin-child id) (string?)") (end-child . "(end-child)") (begin-group . "(begin-group)") (end-group . "(end-group)") (dummy . "(dummy width height)") (draw-circle . "(draw-circle cx cy r col &optional segments thickness)") (draw-arc . "(cx cy r a-min a-max col &optional segments thickness)") (draw-circle-filled . "(cx cy r col &optional segments)") (draw-line . "(x1 y1 x2 y2 col &optional thickness)") (draw-text . "(x y text color)") (color32 . "(color32 r g b &optional alpha) input ranging from 0 to 255
Returns a u32 representation of the color 0xRRGGBBAA") (set-color . "(set-color color-index color-u32)") (color-edit-3 . "(color-edit-3 label *values) *values: aod.c.foreign float[] array") (slider-float . "(slider-float label *value min max &optional (format \"%.3f\"))") (slider-int . "(label *value min max) value: *int pointer from aod.c.foreign/new-int") (input-text . "(input-text label *buffer buffer-size) *buffer is c-pointer to *char from aod.c.foreign/new-char[]") (input-text-multiline . "(input-text-multiline label *buffer buffer-size) *buffer is c-pointer to char* from aod.c.foreign/new-char[]") (combo . "(combo name *index labels)
- *index as returned from aod.c.foreign/new-int
- labels is a 0 separated string. eg \"labelA\\0labelB\\0\\0\"") (is-item-deactivated-after-edit . "IsItemDeactivatedAfterEdit") (is-item-deactivated . "IsItemDeactivated") (set-item-default-focus . "SetItemDefaultFocus") (is-item-focused . "IsItemFocused") (set-keyboard-focus-here . "SetKeyboardFocusHere (&optional offset)
focus keyboard on the next widget. Use positive 'offset' to access sub components of a multiple component widget. Use -1 to access previous widget")) (aod.c.imgui.window-flags "One-to-one relation between ImGuiWindowFlags_* int values.
To use perform bitwise-or and pass the imgui begin as window flags" (None . "") (NoTitleBar . "") (NoResize . "") (NoMove . "") (NoScrollbar . "") (NoScrollWithMouse . "") (NoCollapse . "") (AlwaysAutoResize . "") (NoBackground . "") (NoSavedSettings . "") (NoMouseInputs . "") (MenuBar . "") (HorizontalScrollbar . "") (NoFocusOnAppearing . "") (NoBringToFrontOnFocus . "") (AlwaysVerticalScrollbar . "") (AlwaysHorizontalScrollbar . "") (AlwaysUseWindowPadding . "") (NoNavInputs . "") (NoNavFocus . "") (UnsavedDocument . "") (NoNav . "") (NoDecoration . "") (NoInputs . "")) (aod.c.foreign "Provides a way to create heap allocated primitives like int* float*, int* array, char* array etc
For example, to create a c string use `(new-char[] size)`" (type-bool . "") (new-bool . "creates a heap allocated bool (c-object)") (type-int . "") (new-int . "creates a heap allocated int (c-object)") (type-float . "") (new-float . "creates a heap allocated float (c-object)") (type-bool[] . "") (new-bool[] . "creates a heap allocated bool[] (c-object)") (type-int[] . "") (new-int[] . "creates a heap allocated int[] (c-object)") (type-float[] . "") (new-float[] . "creates a heap allocated float[] (c-object)") (type-char[] . "") (new-char[] . "creates a heap allocated char[] (c-object)")) (aod.c.gl "" (save-screenshot . "(save-screenshot filename) Saves a screenshot of the current gl context")) (aod.c.nfd "Some [nativefiledialog](https://github.com/mlabbe/nativefiledialog) bindings " (open . "(open) Open file dialog. Returns either the selected filename or #f") (save . "(save) Save file dialog. Returns either the selected target filename or #f")) (aod.c.imgui-sdl "Bindings to manually create an SDL_Window and draw to it with imgui. This is to use directly from a simple repl.
ie when no (draw) function is to be called by anyone." (setup . "(setup width height) returns *window
Creates a new SDL_Window, setups opengl, inits imgui") (prepare . "(prepare void*) To be called before calling any ImGui draw functionality") (flush . "(flush void*) To be called after having called any ImGui draw functionality. Paints the window") (destroy . "(destroy *window) Destroys the window & the opengl context")) (aod.c.midi "" (note-on? . "(note-on? status data1 data2)") (note-off? . "(note-off? status data1 data2)") (note-number . "(note-number status data1 data2) Returns either the note or -1")))