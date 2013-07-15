def evalProbit( v )

end;

COEFFS = {
        'age' =>    -1.828641,
        'age_2' =>     2.336877,
        'age_3' =>   -0.9836738,
        'owned_o' =>   -0.1569361,
        'owned_m' =>    -0.584389,
        'socrent' =>   -0.8263617,
        'hq_deg' =>    0.7277306,
        'q_oth' =>    0.2575215,
        'spinhh' =>   -0.3664481,
        'working' =>   -0.1414765,
        'ispc' =>   -0.0131117,
        'disben' =>   -0.5736748,
        'privpen' =>    0.0642335,
        'const' =>     57.81431 }

PERSON = {
        'age' =>    65,
        'age_2' =>  0,
        'age_3' =>   0,
        'owned_o' =>   1,
        'owned_m' =>    0,
        'socrent' =>   0,
        'hq_deg' =>    0,
        'q_oth' =>    0,
        'spinhh' =>   0,
        'working' =>  0,
        'ispc' =>   0,
        'disben' =>  0,
        'privpen' => 0,
        'const' =>   0 }


def wealth( age )
        e = 0;
        p = PERSON;
        p['age'] = age
        p['age_2'] = age*age/100.0
        p['age_3'] = age*age*age/10000.0
        p.each{
                |k,v|
                e += p[k]*COEFFS[k]
        }
        evalProbit( e )
end


  
for age in 60 .. 100 do      
        wealth( age )
end
