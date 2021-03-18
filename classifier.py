import pandas as pd
import re
import nltk
from scipy import sparse
import numpy as np
import tqdm
#import tensorflow as tf
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import sys
np.set_printoptions(threshold=sys.maxsize)


def _filePreprocessing(newdf):
    for row in range(rows):  
        line = newdf[['text']].iloc[row,0]
        listofwords = _removestopwords(line)
        index = 0
        wordcont=len(listofwords)
        for word in listofwords:
        #should cover the case for context 2 words after the focus word which is word
            if (index+1<wordcont and index+2>=wordcont):
                datapoints.append((word, listofwords[index+1]))

            if(index+1<wordcont and index+2<wordcont):
                datapoints.append((word,listofwords[index+1]))
                datapoints.append((word,listofwords[index+2]))

        #should cover the case for context 2 words behind the focus word which is word
            if (index-1<0):
                index+=1 
                continue
    
            if (index-1==0 and index-2<0):
                datapoints.append((word, listofwords[index-1]))
                index+=1
                continue
        
            else:
                datapoints.append((word, listofwords[index-1]))
                datapoints.append((word, listofwords[index-2]))
                index+=1

def _removestopwords(line):
     return[word for word in re.split('\W+', line) if word not in stop_words and word.isdigit() == False]
    
def _createdictionary(dataframe):
    words = []
    index=0
    unique_word_dict = {}
    tokenized_words = dataframe['text'].apply(nltk.word_tokenize)
    rows=tokenized_words.shape[0]
    for line in tokenized_words:
        _words=[word.lower() for word in line if word not in stop_words and word.isdigit() == False]
        for word in _words:
            words.append(word)
   
    for _, word in enumerate(sorted(words)): 
        if unique_word_dict.get(word) == None:
                unique_word_dict.update({word: index})
                index+=1
        else:
             continue    
    return unique_word_dict


def one_hot_encode(unique_word_dict):
    n_words = len(unique_word_dict)
    #words = list(unique_word_dict.keys())
    X = []
    Y = []

    for word_list in datapoints:
        main_word_index = unique_word_dict.get(word_list[0])
        context_word_index = unique_word_dict.get(word_list[1])
        X_row = np.zeros(n_words)
        Y_row = np.zeros(n_words)
        X_row[main_word_index] = 1
        Y_row[context_word_index] = 1
        X.append(X_row)
        Y.append(Y_row)
    X = np.asarray(X)
    Y = np.asarray(Y)
    return [X,Y]

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
        return X
      

from keras.models import Input, Model
from keras.layers import Dense


if __name__ == "__main__":
    stop_words = ['the', 'a', 'and', 'is', 'be', 'will','(', ')','!',
    '#','$','%','^',',','.', '-',':','"','\'', '“', '”', '’','0','1','3','4',
    '5','6','7','8','9'] 
    path = "data/True.csv"
    datapoints = []   
    dataframe = pd.read_csv(path)
    dataframe=dataframe.iloc[1:3]
    dict=_createdictionary(dataframe)
    rows = dataframe.shape[0]
    _filePreprocessing(dataframe)
    print(dataframe)
    X,Y = one_hot_encode(dict)
    #X=tf.convert_to_tensor(X, np.int32)
    #Y=tf.convert_to_tensor(Y, np.int32)

    embed_size = 2

    inp = Input(shape=(X.shape[1],))
    x = Dense(units=embed_size, activation='linear')(inp)
    x = Dense(units=Y.shape[1], activation='softmax')(x)
    model = Model(inputs=inp, outputs=x)
    model.compile(loss = 'categorical_crossentropy', optimizer = 'adam')

    model.fit(
        x=X, 
        y=Y, 
        batch_size=256,
        epochs=1000
        )

    weights = model.get_weights()[0]

    embedding_dict = {}
    for word in words: 
        embedding_dict.update({
            word: weights[unique_word_dict.get(word)]
        })


    #print(X)
    # net = Network()
    # criterion = nn.CrossEntropyLoss()
    # optimizer = optim.SGD(net.parameters(), lr=0.001, momentum=0.9)
    # running_loss = 0.0
    # for epoch in range(1):  # loop over the dataset multiple times
    #     for inputs, outputs in zip(X,Y):
    #         optimizer.zero_grad()
    #         outputs = net(inputs)
    #         loss = criterion(outputs, labels)
    #         loss.backward()
    #         optimizer.step()

    #         running_loss += loss.item()
    #         if i % 1000 == 999:   
    #             running_loss = 0.0
print('Finished Training')