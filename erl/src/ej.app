{application,ej,
    [{description, "open a bridge to a hidden java node"},
     {vsn,"0.0.1"},
     {modules,[f,log,ej_app,ej_sup,ej_srv]},
     {registered,[]},
     {applications,[kernel,stdlib]},
     {env, [{key, value}
           ]},
     {mod,{ej_app,[]}}
    ]
}.