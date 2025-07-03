#C file created using an r4ss function
#C file write time: 2025-07-03  13:11:36
#
1 #_benchmarks
2 #_MSY
0.5 #_SPRtarget
0.4 #_Btarget
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF,  beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)
-999 0 0 0 0 0 -999 0 -999 0
2 #_Bmark_relF_Basis
1 #_Forecast
12 #_Nforecastyrs
1 #_F_scalar
-12345  # code to invoke new format for expanded fcast year controls
# biology and selectivity vectors are updated annually in the forecast according to timevary parameters, so check end year of blocks and dev vectors
# input in this section directs creation of averages over historical years to override any time_vary changes
#_Types implemented so far: 1=M, 4=recr_dist, 5=migration, 10=selectivity, 11=rel. F, recruitment
#_list: type, method (1, 2), start year, end year
#_Terminate with -9999 for type
#_ year input can be actual year, or values <=0 to be rel. styr or endyr
#_Method = 0 (or omitted) means continue using time_vary parms; 1 means to use average of derived factor
 #_MG_type method st_year end_year
        10      1       0        0
        11      1      -3        0
        12      1    -999        0
-9999 0 0 0
3 #_ControlRuleMethod
0.4 #_BforconstantF
0.1 #_BfornoF
1 #_Flimitfraction
3 #_N_forecast_loops
3 #_First_forecast_loop_with_stochastic_recruitment
0 #_fcast_rec_option
1 #_fcast_rec_val
0 #_Fcast_loop_control_5
2037 #_FirstYear_for_caps_and_allocations
0 #_stddev_of_log_catch_ratio
0 #_Do_West_Coast_gfish_rebuilder_output
1999 #_Ydecl
1 #_Yinit
1 #_fleet_relative_F
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
2 #_basis_for_fcast_catch_tuning
# enter list of fleet number and max for fleets with max annual catch; terminate with fleet=-9999
-9999 -1
# enter list of area ID and max annual catch; terminate with area=-9999
-9999 -1
# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999
-9999 -1
2 #_InputBasis
 #_#Year Seas Fleet dead(B)
    2025    1     1    0.09
    2025    1     2    1.21
    2026    1     1    0.09
    2026    1     2    1.41
    2027    1     1    5.13
    2027    1     2    6.87
    2028    1     1    5.33
    2028    1     2    7.03
    2029    1     1    5.48
    2029    1     2    7.12
    2030    1     1    5.58
    2030    1     2    7.15
    2031    1     1    5.62
    2031    1     2    7.13
    2032    1     1    5.63
    2032    1     2    7.07
    2033    1     1    5.61
    2033    1     2    7.00
    2034    1     1    5.56
    2034    1     2    6.91
    2035    1     1    5.50
    2035    1     2    6.83
    2036    1     1    5.43
    2036    1     2    6.74
-9999 0 0 0
#
999 # verify end of input 
