# -*- coding: utf-8 -*-

import os
import socket
import subprocess
from libqtile import bar, layout, widget, qtile, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = 'alacritty'
editor = 'emacs'
browser = 'firefox'
file_manager = 'thunar'

keys = [
    #lauchers
    Key([mod], "Return", lazy.spawn(terminal), desc="Terminal"),
    Key([mod], "w", lazy.spawn(browser), desc="Web browser"),
    Key([mod], "f", lazy.spawn(file_manager), desc="File manager"),
    Key([mod], "e", lazy.spawn(editor), desc="Emacs"),
    
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "mod1"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "mod1"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
]

groups = [Group(i) for i in ['','','','','','','','','']]
group_bind_keys = '123456789'

for g, k in zip(groups, group_bind_keys):
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                k,
                lazy.group[g.name].toscreen(),
                desc=f"Switch to group {g.name}",
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                k,
                lazy.window.togroup(g.name, switch_group=True),
                desc=f"Switch to & move focused window to group {g.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

colors_theme = [
    '#2e3440',
    '#3b4252',
    '#434c5e',
    '#4c566a',
    '#8fbcbb',
    '#88c0d0',
    '#81a1c1',
    '#5e81ac',
    '#bf616a',
    '#d08770',
    '#ebcb8b',
    '#a3be8c',
    '#b48ead',
    '#f8f8f8',
]
    
layout_theme = {
    "border_width": 2,
    "margin": 3,
    "border_focus": colors_theme[12],
    "border_normal": colors_theme[3]
}

layouts = [
    layout.MonadTall(**layout_theme, ratio = 0.50),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2),
    layout.RatioTile(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.Floating(**layout_theme),
]

prompt = '{0}@{1}: '.format(os.environ['USER'], socket.gethostname())

widget_defaults = dict(
    font='JetBrainsMono Nerd Font Bold',
    fontsize=11,
    padding=2,
    background=colors_theme[0]
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(fontsize = 17,
                                margin_x = 2,
                                margin_y = 3,
                                padding_x = 5,
                                padding_y = 5,
                                boderwidth = 3,
                                rounded = False,
                                active = colors_theme[13],
                                inactive = colors_theme[0],
                                highlight_color = colors_theme[3],
                                highlight_method = 'line',
                                this_current_screen_border = colors_theme[8],
                                this_screen_border = colors_theme[7],
                                other_current_screen_border = colors_theme[8],
                                other_screen_border = colors_theme[7],
                                foreground = colors_theme[13],
                                background = colors_theme[2]),
                
                widget.Prompt(),

                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.WindowName(font = 'JetBrainsMono Nerd Font',
                                  foreground = colors_theme[13],
                                  background = colors_theme[1],
                                  padding = 0),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.CurrentLayoutIcon(custom_icon_paths = [os.path.expanduser('~/.config/qtile/icons')],
                                         foreground = colors_theme[0],
                                         background = colors_theme[6],
                                         padding = 0,
                                         scale = 0.7),
                widget.CurrentLayout(font = 'JetBrainsMono Nerd Font Bold',
                                     fmt = '{}',
                                     max_chars = 2,
                                     foreground = colors_theme[0],
                                     background = colors_theme[6],
                                     padding = 2),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.TextBox(font = 'JetBrainsMono Nerd Font',
                               text = '',
                               background = colors_theme[10],
                               foreground = colors_theme[0],
                               padding = 4,
                               fontsize = 13),                
                widget.Memory(format='{MemUsed:.0f}{mm}',
                              mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e htop')},
                              update_interval = 10,
                              padding = 2,
                              margin = 5,
                              background = colors_theme[10],
                              foreground = colors_theme[0]),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.TextBox(font = 'JetBrainsMono Nerd Font',
                               text = '',
                               background = colors_theme[9],
                               foreground = colors_theme[0],
                               padding = 4,
                               fontsize = 13),
                widget.Clock(format = '%Y-%m-%d, %H:%M',
                             background = colors_theme[9],
                             foreground = colors_theme[0]),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.TextBox(font = 'JetBrainsMono Nerd Font',
                               text = '墳',
                               background = colors_theme[11],
                               foreground = colors_theme[0],
                               padding = 4,
                               fontsize = 13),               
                widget.Volume(fmt = '{}',
                              padding = 5,
                              margin = 3,
                              background=colors_theme[11],
                              foreground=colors_theme[0]),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),
                widget.Systray(background = colors_theme[8], padding = 5),
                widget.CheckUpdates(update_interval = 3600,
                                    distro='Arch',
                                    display=': {updates}',
                                    mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(terminal + ' -e sudo pacman -Syu')},
                                    foreground = colors_theme[0],
                                    colour_have_updates = colors_theme[0],
                                    colour_no_updates = colors_theme[0],
                                    padding = 3,
                                    background = colors_theme[12]),
                widget.Sep(padding=3, linewidth=0, background=colors_theme[0]),

                widget.QuickExit(),

            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.Popen([home])

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
