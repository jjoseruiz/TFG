valoresImagen<-function(IMAGEN,COORDENADAS){
  #vecinos_xvoxel vecinos por 3 coordenadas cada uno.
  vecinos_xvoxel=125
  if(!is.antsImage(IMAGEN)){
    imagenants = as.antsImage(IMAGEN)
  }else{
    imagenants = IMAGEN
  }
  if(is.matrix(COORDENADAS)){
    coord = matrix(nrow = nrow(COORDENADAS),ncol = vecinos_xvoxel,byrow = TRUE)
    for(i in 1:nrow(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = c(COORDENADAS[i,1],COORDENADAS[i,2],COORDENADAS[i,3]),c(2,2,2))
      coordi = ventana$values
      coord[i,1:length(coordi)]=coordi
      if(i==round(nrow(COORDENADAS)/2))
        print(paste0("voxel nº = ",i,". vas por la mitad."))
    }
     # 
      #coordi = ventana$indices
      #ordenada = c()
      #for(j in 1:nrow(coordi)){
      #  ordenada=c(coordi[j,1:3],ordenada)
      #}
      #cada tres columnas tenemos las coordenadas de un vecino
      #coord[i,1:length(ventana$values)]=ordenada
      #if(i==length(COORDENADAS)/2){
       # print(paste0("voxel nº = ",i,". vas por la mitad."))
    #}
    #}
    #hay que tener en cuenta que los primeros numvoxel/2 son LESION
    #Los numVoxel/2 siguientes son SANOS
    return(coord)
  }else{
    #DEVUELVE DIRECTAMENTE EL VALOR DE LOS VECINOS
    coord = matrix(nrow = length(COORDENADAS),ncol = vecinos_xvoxel)
    for(i in 1:length(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = obtenCoord(COORDENADAS[i],imagenants),c(2,2,2))
      coordi = ventana$values
      coord[i,1:length(coordi)]=coordi
      if(i==round(length(COORDENADAS)/2))
        print(paste0("voxel nº = ",i,". vas por la mitad."))
    }
    return(coord)
  }
}