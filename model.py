import torch.nn as nn
import torch.nn.functional as F

'''
NN accepts 21 forwards 2 for 
linear activation 
NN accepts 2 and forwards 21 for
softmax activation 

'''
class Network(nn.Module):

    def __init__(self):
        super().__init__()
        self.fc1 = nn.Linear(21, 2)
        self.fc2 = nn.Linear(2, 21)

    def forward(self,x):
        x = self.fc1(x)
        x = self.fc2(x)
        X = F.softmax(x)
      


