TYPE directive = (init, entr, prnt, save, load, quit, help, badc);
     categories_and_totals =
      (air_fare, car_rental, personal_auto_milage, parking,
                  local_transportation,  lodging, breakfast, lunch,  
                  dinner, entertainment, tips, telephone, miscellaneous,
                  category_total, category_co_paid, category_emp_paid);
     days_and_totals =
            (sun, mon, tue, wed, tur, fri, sat,
                  day_total, day_co_paid, day_emp_paid);
     category = air_fare .. miscellaneous;
     days     = sun .. sat;
     method_of_payment = (employee_paid, company_paid);
TYPE exp_record = RECORD
     date_prepared : STRING [9];
     week_ending_date : STRING [9];
     auto_milage_rate : REAL;
     currency_conversion_factor : REAL;
     name : STRING [30];
     comment : ARRAY [1..4] OF STRING [60];
     expenses : ARRAY [category, days] of RECORD
         expense : REAL;
         how_paid : method_of_payment;
         END;
END;
