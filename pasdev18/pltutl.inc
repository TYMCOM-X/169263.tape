$include pltutl.typ[52250,240]

external var plt: plt_state;

external procedure plt_init ( pltr_name: string; var err: boolean );
external procedure plt_on;
external procedure plt_off;
external procedure plt_layout ( blx, bly, urx, ury: real );
external procedure plt_window ( blf: point; urb: point );
external procedure plt_scale ( sc: xyz );
external procedure plt_translate ( tr: xyz );
external procedure plt_rotate ( rot: xyz );
external procedure plt_move ( pt: point );
external procedure plt_draw ( pt: point );
external procedure plt_color ( color: pen_colors );
external procedure plt_speed ( speed: real );
    