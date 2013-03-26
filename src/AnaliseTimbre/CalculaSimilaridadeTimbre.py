'''
Created on 21/03/2013

@author: giovanicb
'''

import sys
import numpy
import os
#import tables
from numpy import genfromtxt

if __name__ == '__main__':    
    TAMANHO_TUPLA = 12
    path = "./timbres/"
    playlists = list()
    timbres = list()
    index = 0
    for row in open('play-timbre.txt', 'r'):
        playlist, timbre = row.split('\t')
        playlists.insert(index, playlist.replace("\"", "").strip())
        timbres.insert(index, timbre.replace("\"", "").strip())
        index = index + 1

    saida = open("similaridades.txt", "w")
    TAMANHO_LISTA = range(0, len(playlists) - 2)
    for i in TAMANHO_LISTA:
        play_a = playlists[i]
        play_b = playlists[i + 1]
        timbre_a = timbres[i]
        timbre_b = timbres[i + 1]
        if(play_a != play_b and i):            
            play_a = play_b
            timbre_a = timbre_b
            play_b = playlists[i + 2]
            timbre_b = timbres[i + 2]           
        h5_A = genfromtxt(timbre_a, delimiter=',')
        h5_B = genfromtxt(timbre_b, delimiter=',')       
        if(len(h5_A) > 0 and len(h5_B) > 0):
            h5_A_transpose = h5_A.transpose()
            h5_B_transpose = h5_B.transpose()

            music_A_timbre_T = h5_A_transpose

            average_A = numpy.average(music_A_timbre_T, axis=1)

            covariance_matrix_A = numpy.cov(music_A_timbre_T)

            inv_covariance_matrix_A = numpy.linalg.inv(covariance_matrix_A)                
                    
            music_B_timbre_T = h5_B_transpose
            average_B = numpy.average(music_B_timbre_T, axis=1)
            covariance_matrix_B = numpy.cov(music_B_timbre_T)
            inv_covariance_matrix_B = numpy.linalg.inv(covariance_matrix_B)
             
                
            trace1 = numpy.trace(numpy.dot(covariance_matrix_A, inv_covariance_matrix_B))
            trace2 = numpy.trace(numpy.dot(covariance_matrix_B, inv_covariance_matrix_A))
            trace3 = numpy.trace((inv_covariance_matrix_A + inv_covariance_matrix_B) + (numpy.dot((average_A - average_B), ((average_A - average_B).transpose()))))
            distance = trace1 + trace2 + trace3 - 2 * TAMANHO_TUPLA
            saida.write(play_a + "\t" +timbre_a[10:len(timbre_a) - 4] + "\t" + timbre_b[10:len(timbre_b) - 4] + "\t" + str(distance) + "\n")
    saida.close()
pass
