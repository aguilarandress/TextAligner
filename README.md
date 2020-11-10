# TextAligner

> Aplicación en consola para alinear texto en líneas del mismo tamaño

## Quick start

```bash
# Compilar aplicación y crear ejecutable
$ ghc Main.hs --make -o YOUR_APP_NAME
```

## Comandos

```bash
# Carga un diccionario de palabras
>> load YOUR_FILENAME.txt

# Muestra el diccionario de palabras
>> show

# Separa el texto en lineas del mismo largo
>> split N SEPARAR ALINEAR [TEXTO] # N debe ser entero y SEPARAR y ALINEAR deben ser 'n' o 's'

# Igual que split pero la entrada es un archivo (Archivo 2 es opcional)
>> splitf LONGITUD SEPARAR ALINEAR file1.txt file2.txt

# Guarda el diccionario de palabras en el archivo ingresado
>> save FILE_NAME.txt

# Termina el programa
>> exit
```

## Archivos de prueba

En el folder `_resources/` se encuentran archivos de prueba para cargar dentro de la aplicación

## Curso

Lenguajes de programación

## Application info

### Version

1.0.0

### Author

Andrés Aguilar Moya
