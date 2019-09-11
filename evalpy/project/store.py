from collections import defaultdict


class RunStore:

    def __init__(self):
        self.param_store = {}
        self.progress_store = {}


class ProgressStore:

    def __init__(self):
        self.progress = {}

    def push_progress(self, progress):
        pass
