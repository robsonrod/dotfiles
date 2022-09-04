# -*- coding: utf-8 -*-

import os
import socket
import subprocess
import re

from libqtile import bar, layout, widget, qtile, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, KeyChord
from libqtile.lazy import lazy

mod = 'mod4'
shift = 'shift'
control = 'control'
alt = 'mod1'

terminal = 'alacritty'
editor = 'emacs'
editor2 = 'code'
browser = 'firefox'
file_manager = 'thunar'
rofi = 'rofi -modi drun -show drun -line-padding 4 -columns 2 -padding 50 -hide-scrollbar -show-icons -drun-icon-theme "Papirus-Dark"'
screenshot = 'xfce4-screenshooter'
peek = 'peek'
rofi_run = 'rofi -modi run -show run -p "run:"'
rofi_window = 'rofi -modi window -show window -i -p -config ~/.config/rofi/themes/dt-center.rasi -fixed-num-lines: false'

keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "space", lazy.layout.next()),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, shift], "h", lazy.layout.shuffle_left()),
    Key([mod, shift], "l", lazy.layout.shuffle_right()),
    Key([mod, shift], "j", lazy.layout.shuffle_down()),
    Key([mod, shift], "k", lazy.layout.shuffle_up()),

    # Grow windows
    Key([mod, control], "l", lazy.layout.grow()),
    Key([mod, control], "h", lazy.layout.shrink()),
    Key([mod, control], "n", lazy.layout.normalize()),
    Key([mod, control], "o", lazy.layout.maximize()),
    Key([mod, control], "space", lazy.layout.flip()),

    # Toggle between different layouts
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "Tab", lazy.prev_layout()),

    # Keyboard layout select
    Key([mod, alt], "k", lazy.spawn('keyboardlayout')),
    
    # Lauchers
    Key([mod], "Return", lazy.spawn(terminal)),
    Key([mod], "b", lazy.spawn(browser)),
    Key([mod], "f", lazy.spawn(file_manager)),
    Key([mod], "m", lazy.spawn(rofi)),
    Key([mod], "r", lazy.spawn(rofi_run)),
    Key([mod], "w", lazy.spawn(rofi_window)),
    Key([mod], "x", lazy.spawn('xprop'), desc="List window properties"),
    Key([mod], "s", lazy.spawn(screenshot)),
    Key([mod, alt], "l", lazy.spawn('screen_lock')),

    KeyChord([mod],"p", [
        Key([], "e", lazy.spawn("emacsclient -c -a 'emacs'")),
        Key([], "c", lazy.spawn(editor2), desc="Code"),        
    ]),

    KeyChord([mod],"u", [
        Key([], "p", lazy.spawn(peek)),
        Key([], "s", lazy.spawn('spotify')),
        Key([], "v", lazy.spawn('vlc')),
    ]),

    KeyChord([mod],"d", [
        Key([], "m", lazy.spawn('dmenumount')),
        Key([], "u", lazy.spawn('dmenuumount')),
    ]),
       
    # Audio control
    Key([], "XF86AudioMute", lazy.spawn("pactl set-sink-mute 0 toggle")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume 0 +5%")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume 0 -5%")),

    # Qtile operations
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, alt], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, alt], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, alt, shift], "q", lazy.spawn(os.path.expanduser('powermenu')), desc="Powermenu"),
]

groups = [Group(i) for i in ['', '', '', '', '', '', '', '', '']]
group_bind_keys = '123456789'

for g, k in zip(groups, group_bind_keys):
    keys.extend(
        [
            Key([mod], k, lazy.group[g.name].toscreen()),
            Key([mod, "shift"], k, lazy.window.togroup(g.name, switch_group=True)),
        ]
    )

theme = {
    "background":   '#181818',
    "current_line": '#282828',
    "foreground":   '#f8f8f2',
    "comment":      '#535453',
    "black":        '#181818',
    "red":          '#ab4642',
    "green":        '#a1b56c',
    "yellow":       '#f7ca88',
    "blue":         '#7cafc2',
    "magenta":      '#ba8baf',
    "cyan":         '#86c1b9',
    "white":        '#d8d8d8',
    "purple":       '#bd93f9',
    "pink":         '#ff79c6',
    "red2":         '#ff5555',
    "green2":       '#50fa7b',
}

layout_theme = {
    "border_width": 2,
    "margin": 2,
    "border_focus": theme['magenta'],
    "border_normal": theme['comment']
}

layouts = [
    layout.MonadTall(**layout_theme, ratio=0.50),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2),
    layout.RatioTile(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.Floating(**layout_theme),
]

prompt = '{0}@{1}: '.format(os.environ['USER'], socket.gethostname())

widget_defaults = dict(
    font='JetBrainsMono Nerd Font',
    fontsize=10,
    padding=2,
    margin=21,
    background=theme['background']
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.CurrentLayoutIcon(custom_icon_paths=[os.path.expanduser('~/.config/qtile/icons')],
                                         foreground=theme['background'],
                                         background=theme['background'],
                                         padding=0,
                                         scale=0.7),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['foreground'],
                           padding=5),

                widget.GroupBox(fontsize=13,
                                margin_x=0,
                                margin_y=3,
                                padding_x=3,
                                padding_y=5,
                                boderwidth=1,
                                rounded=False,
                                active=theme['foreground'],
                                inactive=theme['comment'],
                                highlight_color=theme['background'],
                                highlight_method='text',
                                urgent_alert_method = 'text',
                                urgent_text=theme['purple'],
                                this_current_screen_border=theme['red2'],
                                this_screen_border=theme['red2'],
                                other_current_screen_border=theme['red2'],
                                other_screen_border=theme['red2'],
                                foreground=theme['background'],
                                background=theme['background']),

                widget.TextBox(
                    text='',
                    background=theme['current_line'],
                    foreground=theme['background'],
                    padding=0,
                    fontsize=18
                ),
                widget.Sep(linewidth=0,
                           background=theme['current_line'],
                           foreground=theme['current_line'],
                           padding=5),
                
                widget.WindowName(font='JetBrainsMono Nerd Font',
                                  max_chars=200,
                                  foreground=theme['foreground'],
                                  background=theme['current_line'],
                                  padding=0),
                widget.Sep(
                    linewidth=0,
                    padding=5,
                    foreground=theme['background'],
                    background=theme['background']
                ),
                widget.TextBox(
                    font="Iosevka Bold",
                    text='',
                    background=theme['cyan'],
                    foreground=theme['background'],
                    padding=5,
                    fontsize=21
                ),
                widget.ThermalSensor(
                    font="JetBrainsMono Bold",
                    fontsize=10,
                    foreground=theme['background'],
                    background=theme['white'],
                    threshold=90,
                    fmt='{}',
                    padding=5
                ),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='',
                               background=theme['red'],
                               foreground=theme['background'],
                               padding=4,
                               fontsize=21),
                widget.Memory(font='JetBrainsMono Nerd Font Bold',
                    format='{MemUsed:.0f}{mm}',
                              mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                                  terminal + ' -e htop')},
                              update_interval=10,
                              padding=2,
                              margin=5,
                              background=theme['foreground'],
                              foreground=theme['background']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='',
                               background=theme['blue'],
                               foreground=theme['background'],
                               padding=4,
                               fontsize=21),
                widget.CPU(font='JetBrainsMono Nerd Font Bold',
                           format='{load_percent}%', mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                               terminal + ' -e htop')},
                           update_interval=10,
                           padding=2,
                           margin=5,
                           background=theme['foreground'],
                           foreground=theme['background']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='墳',
                               mouse_callbacks={
                                   'Button1': lambda: qtile.cmd_spawn('pavucontrol')},
                               background=theme['yellow'],
                               foreground=theme['background'],
                               padding=4,
                               fontsize=21),
                widget.PulseVolume(font='JetBrainsMono Nerd Font Bold',
                                   fmt='{}',
                                   padding=5,
                                   margin=3,
                                   limit_max_volume=True,
                                   background=theme['foreground'],
                                   foreground=theme['background']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='',
                               background=theme['magenta'],
                               foreground=theme['background'],
                               padding=4,
                               fontsize=21),
                widget.Clock(font='JetBrainsMono Nerd Font Bold',
                             format='%Y-%m-%d, %H:%M',
                             mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                                 browser + ' https://calendar.google.com')},
                             background=theme['foreground'],
                             foreground=theme['background']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='',
                               background=theme['green'],
                               foreground=theme['background'],
                               padding=4,
                               fontsize=21),
                widget.CheckUpdates(font='JetBrainsMono Nerd Font Bold',
                                    update_interval=1800,
                                    distro='Ubuntu',
                                    display_format='Updates : {updates}',
                                    no_update_string = "Updates : 0",
                                    mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                                        terminal + ' -e sudo apt update')},
                                    colour_have_updates=theme['background'],
                                    colour_no_updates=theme['background'],
                                    padding=3,
                                    foreground=theme['background'],
                                    background=theme['foreground']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),
                widget.TextBox(font='Iosevka Bold',
                               text='',
                               background=theme['blue'],
                               foreground=theme['background'],
                               padding=6,
                               fontsize=21),
                widget.KeyboardLayout(font='JetBrainsMono Nerd Font Bold',
                                      configured_keyboards=['us', 'us alt-intl'],
                                      padding=5,
                                      background=theme['foreground'],
                                      foreground=theme['background']),
                widget.Sep(linewidth=0,
                           background=theme['background'],
                           foreground=theme['background'],
                           padding=5),               
                widget.Systray(background=theme['background'], padding=5, icon_size=20),

                widget.Sep(
                    linewidth=0,
                    padding=5,
                    foreground=theme['background'],
                    background=theme['background']
                ),
                
            ],
            21,
            #border_width=[0, 0, 21, 0],  # Draw top and bottom borders
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

def send_window_to_group_by_re(window, wmname=None, wmclass=None, role=None, group=None):
    if not (wmname or wmclass or role):
        raise TypeError(
            "at least one of name, wmclass or role must be specified"
        )

    if not group:
        raise TypeError(
            "group must be specified"
        )
    
    if wmname and re.match(wmname, window.name):
        window.togroup(group)

    if wmclass:
        cls = window.window.get_wm_class()
        if cls:
            for v in cls:
                if (re.match(wmclass, v)):
                    window.togroup(group)

    if role:
        rol = window.window.get_wm_window_role()
        if rol and re.match(role, rol):
            window.togroup(group)

@hook.subscribe.client_new
def group_app(window):
    send_window_to_group_by_re(window, wmclass='Thunar', group='')
    send_window_to_group_by_re(window, wmclass='Emacs', group='')
    send_window_to_group_by_re(window, wmclass='microsoft teams - preview', group='')
    send_window_to_group_by_re(window, wmclass='Code', group='')
    send_window_to_group_by_re(window, wmname='Mozilla Firefox', group='')
    send_window_to_group_by_re(window, wmclass='Spotify', group='')
    send_window_to_group_by_re(window, wmclass='VirtualBox Manager', group='')
    send_window_to_group_by_re(window, wmclass='Simulador', group='')
    
@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    bubble = window.window.get_wm_window_role() == 'bubble'
    transient = window.window.get_wm_transient_for()
    if dialog or transient or bubble:
        window.floating = True
    
dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="peek"),  # ssh-askpass
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class='pavucontrol'),
        Match(wm_class='feh'),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title="Simulador"),
        Match(wm_class="microsoft teams - preview"),
        Match(wm_class="VirtualBox Manager")
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
