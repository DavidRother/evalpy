from evalpy.project import client

__client = client.Client()

start_run = __client.start_run
end_run = __client.end_run
set_project = __client.set_project
log_run_entries = __client.log_entries
log_run_step = __client.log_step
forward_run_step = __client.forward_step


__all__ = ["start_run", "end_run", "set_project", "log_run_entries", "log_run_step", "forward_run_step"]
