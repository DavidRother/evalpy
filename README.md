# evalpy
A lightweight framework for experiment logging and automatic visualization

Evalpy aims at researchers that want a fast and efficient framework to log their experiment configurations alongside
the results. This includes an interface to both log the parameters and metrics of any single run as well as the progression during a run
in a time series manner.

The second part of evalpy includes the GUI which is provided within the package.
To start the GUI simply activate the environment in which you installed evalpy in a console
and execute the following:

    evalpy run
    
Quickstart
-------------------------------

The intended usage involves the following steps:


- Declaring the project root, a file path
- Declaring the project name, the name of the project directory
- Starting a run with an experiment name
- In the run one can log one time the parameters and metrics 
and do a step logging for the run progression

A minimal usage outline is as follows

    import evalpy
    
    
    evalpy.set_project('my_first_project_path', 'my_project_folder_name')
    with evalpy.start_run('experiment_name'):
        for log_step_stuff in model_training():
            evalpy.log_run_step(log_step_stuff, step_forward=True)  
        evalpy.log_run_entries(model_parameters_and_metrics)  # both methods expect a dict as input