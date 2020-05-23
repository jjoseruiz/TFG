


i=0
j=0
#creamos matriz de almacenamiento de los índices de tamaño numvoxel filas y 3 columnas
indices = matrix(nrow = num_voxel,ncol = 3) 
while(i<num_voxel/2 | j<num_voxel/2){
  #ponemos que empiecen en 5 para que no coja voxeles en los extremos de la imagen
  #generamos valores aleatorios
  coordenada = c(sample(5:187,1),sample(5:507,1),sample(5:507,1))
  if(!estaEnLista(indices,coordenada)){
    #si no esta en la lista
    #añado a mi lista de voxeles
    #¿cómo se si es de lesion o no? sí lo es, es muy probable que en sus alrededores haya mas voxeles enfermos
    vecinos = getNeighborhoodAtVoxel(ants_mascara,as.numeric(coordenada),c(2,2,2))
    #creamos la lista de valores que afirman si los vecinos son enfermos o no.
    vecinos_enfermos=vecinos$values>0
    #si el central es enfermo entramos aquí
    if(ants_mascara[coordenada[1],coordenada[2],coordenada[3]][1]>0){
      l=0
      while(l<length(vecinos$values)){
        if(l>0){
          if(vecinos_enfermos[l] & i<num_voxel/2 & !estaEnLista(indices,vecinos$indices[l,1:3])){
            i=i+1
            print(paste0("voxel ENFERMO",i))
            indices[i+j,1:ncol(indices)]=vecinos$indices[l,1:3]
          }else{
            if(j<num_voxel/2 & !estaEnLista(indices,vecinos$indices[l,1:3])){
              j=j+1
              print(paste0("voxel SANO",j))
              indices[i+j,1:ncol(indices)]=vecinos$indices[l,1:3]
            }
          }
        }
        l=l+1
      }
    }else{
      if(j<num_voxel/2 & !estaEnLista(indices,coordenada)){
        j=j+1
        print(paste0(("voxel SANOK"),j))
        indices[i+j,1:ncol(indices)]=coordenada
      }
    }
  }
}
return(indices)
}
}







