#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set repeat.initial_wait 300
/bin/echo -n .
$cli set option.emacsmode_controlV 1
/bin/echo -n .
$cli set option.emacsmode_controlPNBF_ex 1
/bin/echo -n .
$cli set option.emacsmode_controlY 1
/bin/echo -n .
$cli set option.emacsmode_controlK_ex 1
/bin/echo -n .
$cli set repeat.wait 53
/bin/echo -n .
$cli set option.emacsmode_ex_controlU_delete 1
/bin/echo -n .
$cli set remap.holdcommandQ 1
/bin/echo -n .
$cli set remap.mouse_keys_mode_2 1
/bin/echo -n .
$cli set option.emacsmode_controlS 1
/bin/echo -n .
$cli set parameter.mouse_key_scroll_natural_direction 1
/bin/echo -n .
/bin/echo
