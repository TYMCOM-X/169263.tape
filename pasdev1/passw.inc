external function switch (head_sw: switch_ptr; switch_name: switch_string): boolean;
external function enable_switch (head_sw: switch_ptr; switch_name: switch_string; enable: boolean): switch_ptr;
external procedure pop_switches (head_sw: switch_ptr; final_sw: switch_ptr);
external procedure sw_save (head_sw: switch_ptr; var f: file of * );
external function sw_load (var f: file of * ): switch_ptr;
   