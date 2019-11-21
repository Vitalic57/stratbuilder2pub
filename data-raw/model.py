from basemodel import BaseModel

# Class should be named Model
class Model(BaseModel):
    
    def __init__(self):
        self.current = 0
        self.prev = 0
    
    def init(self, x):
        # self.model = my_lib.my_model(arg1, arg2)
        # self.model.fit(x)
        pass
        # self.prev = x[-2][0]
        # self.current = x[-1][0]
    
    def predict(self):
        if self.current > self.prev:
            return 0.
        else:
            return 1.

    def step(self, x):
        # self.prev = self.current
        # self.current = x[0]
        self.current = x[1][0]
        self.prev = x[0][0]
        
        
