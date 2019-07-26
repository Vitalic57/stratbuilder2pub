from abc import abstractmethod, ABC

class BaseModel(ABC):

    @abstractmethod
    def init(self, x):
        """
        Here the model is initialized
        """
        pass


    @abstractmethod
    def step(self, x):
        """
        Here you could if you wish implement the method which update the model
        once new raw of data has come
        """
        pass


    @abstractmethod
    def predict(self):
        """
        Here you must implement the function which predict probability of the next move to be up
	      depending on the state inside the model
        """
        pass
