{application,ej,
    [{description, "handle a neo4j database"},
     {vsn,"0.0.1"},
     {modules,[f,log,neo4j_app,neo4j_sup,neo4j]},
     {registered,[]},
     {applications,[kernel,stdlib,ej]},
     {env, [{key, value}
           ]},
     {mod,{neo4j_app,[]}}
    ]
}.