import ply.lex as lex
import re

reserved = {
    '"To do"': 'TO_DO',
    '"In progress"': 'INPROGRESS',
    '"Canceled"': 'CANCELED',
    '"Done"': 'DONE',
    '"On hold"': "ON_HOLD",
    '"Product Analyst"': 'PRODUCT_ANALYST',
    '"Project Manager"': 'PROJECT_MANAGER',
    '"UX designer"': 'UXDESIGNER',
    '"Marketing"': 'MARKETING',
    '"Developer"': 'DEVELOPER',
    '"Devops"': 'DEVOPS',
    '"DB admin"': 'DB_ADMIN',
    '"empresas"': 'EMPRESAS',
    '"fundación"': 'FUNDACION',
    '"dirección"': 'DIRECCION',
    '"calle"': 'CALLE',
    '"ciudad"': 'CIUDAD',
    '"país"': 'PAIS',
    '"ingresos_anuales"': 'INGRESOS_ANUALES',
    '"pyme"': 'PYME',
    '"link"': 'LINK',
    '"departamentos"': 'DEPARTAMENTOS',
    '"nombre"': 'NOMBRE',
    '"jefe"': 'JEFE',
    '"subdepartamentos"': 'SUBDEPARTAMENTOS',
    '"empleados"': 'EMPLEADOS',
    '"edad"': 'EDAD',
    '"cargo"': 'CARGO',
    '"salario"': 'SALARIO',
    '"activo"': 'ACTIVO',
    '"fecha_contratación"': 'FECHA_CONTRATACION',
    '"proyectos"': 'PROYECTOS',
    '"fecha_inicio"': 'FECHA_INICIO',
    '"fecha_fin"': 'FECHA_FIN',
    '"version"': 'VERSION',
    '"firma_digital"': 'FIRMA_DIGITAL',
}

tokens = (
    'URL', 'DATE', 'INTEGER', 'FLOAT', 'BOOLEAN',
    'ILLAVE', 'DLLAVE', 'ICORCHETE', 'DCORCHETE', 'IPARENT', 'DPARENT',
    'COMA', 'DOSPUNTOS', 'NOMBREPROPIO', 'STRING', 'NULL','VERSIONE',
) + tuple(reserved.values())

t_ILLAVE = r'\{'
t_DLLAVE = r'\}'
t_ICORCHETE = r'\['
t_DCORCHETE = r'\]'
t_IPARENT = r'\('
t_DPARENT = r'\)'
t_COMA = r'\,'
t_DOSPUNTOS = r'\:'

def t_URL(t):
    r'"(https?|ftp)://[^\s/$.?#].[^\s]*"'
    match = re.match(r'"(https?|ftp)://([^\s/:]+)(:\d+)?(/.*)?"', t.value)
    if match:
        protocolo, dominio, puerto, ruta = match.groups()
        if protocolo not in ['http', 'https']:
            print(f"Error protocolo de red invalido: {protocolo}")
            t.lexer.skip(len(t.value))  # Saltar el token completo
        else:
            t.value = {
                'protocolo': protocolo,
                'dominio': dominio,
                'puerto': puerto.lstrip(':') if puerto else 80,
                'ruta': ruta if ruta else '/'
            }
            t.type = 'URL'
            return t
    else:
        print(f"URL inválida: {t.value}")
        t.lexer.skip(len(t.value))  # Saltar el token completo

def t_DATE(t):
    r'"\b(19[0-9]{2}|20[0-9]{2})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\b"'
    match = re.match(r'"\b(19[0-9]{2}|20[0-9]{2})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\b"', t.value)
    if match:
        year, month, day = map(int, match.groups())
        if year > 2099 or year < 1900:
            print(f"Año fuera de rango: {year}")
            t.lexer.skip(len(t.value))  # Saltar el token completo
        elif month > 12 or month < 1:
            print(f"Mes fuera de rango: {month}")
            t.lexer.skip(len(t.value))  # Saltar el token completo
        elif day > 31 or day < 1:
            print(f"Día fuera de rango: {day}")
            t.lexer.skip(len(t.value))  # Saltar el token completo
        else:
            t.value = {'year': year, 'month': month, 'day': day}
            t.type = 'DATE'
            return t
    else:
        print(f"Fecha inválida: {t.value}")
        t.lexer.skip(len(t.value))  # Saltar el token completo

def t_NULL(t):
    r'null'
    t.value = None  # Asignar None al valor del token
    t.type = 'NULL'
    return t

def t_VERSIONE(t):
    r'"\d+\.\d{2}"|"\d+\.\d{1}"'
    t.type = 'VERSIONE'
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)  # Convertir el valor a float
    t.type = 'FLOAT'
    return t

def t_INTEGER(t):
    r'-?\d+'
    if int(t.value) < 0:
        print("Error, no se aceptan números negativos!!")
        t.lexer.skip(len(t.value))  # Saltar el token completo
    else:
        t.value = int(t.value)  # Convertir el valor a int
        t.type = 'INTEGER'
        return t

def t_BOOLEAN(t):
    r'true|false'
    t.value = (t.value == 'true')  # Convertir a valor booleano
    t.type = 'BOOLEAN'
    return t

def t_NOMBREPROPIO(t):
    r'"[A-Z]+([a-z]+)\s [A-Z]+([a-z]+)"'
    if t.value in reserved:
        t.type = reserved.get(t.value)
    else:
        t.type = 'NOMBREPROPIO'
    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'

    # Aquí se verifican todas las demás reglas
    if re.match(r'(https?|ftp)://[^\s/$.?#].[^\s]*', t.value):
        return t_URL(t)
    elif re.match(r'"\b(19[0-9]{2}|20[0-9]{2})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\b"', t.value):
        if match:
            year, month, day = map(int, match.groups())
            if year > 2099 or year < 1900:
                print(f"Año fuera de rango: {year}")
                t.lexer.skip(len(t.value))  # Saltar el token completo
            elif month > 12 or month < 1:
                print(f"Mes fuera de rango: {month}")
                t.lexer.skip(len(t.value))  # Saltar el token completo
            elif day > 31 or day < 1:
                print(f"Día fuera de rango: {day}")
                t.lexer.skip(len(t.value))  # Saltar el token completo
            else:
                t.value = {'year': year, 'month': month, 'day': day}
                t.type = 'DATE'
            return t_DATE(t)
    elif re.match(r'"\d+\.\d{2}"|"\d+\.\d{1}"',t.value):
        return t_VERSIONE(t)
    elif re.match(r'null', t.value):
        return t_NULL(t)
    elif re.match(r'"\d+\.\d+"', t.value):
        return t_FLOAT(t)
    elif re.match(r'-?\d+', t.value):
        return t_INTEGER(t)
    elif re.match(r'true|false', t.value):
        return t_BOOLEAN(t)
    
    # Si no coincide con ninguna de las anteriores, es STRING
    if t.value in reserved:
        t.type = reserved.get(t.value)
    else:
        t.type = 'STRING'
    
    return t

# Ignorar espacios en blanco y tabulaciones
t_ignore = ' \t\n'

# Manejo de errores
def t_error(t):
    print(f"Caracter ilegal '{t.value[0]}'")
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()

print(f"Bienvenido al Analizador léxico de Automatas Infinitos CREW.")

def mostrar_menu():
    print("Menú de opciones")
    print("1. Realizar el análisis léxico de forma manual")
    print("2. Analizar un archivo.json específico")
    print("3. Salir")

def analizar_entrada_manual():
    data = input("Ingrese el texto a analizar: ")
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(f"Se ha encontrado el token: '{tok.value}' del tipo: {tok.type}")

def analizar_archivo():
    ruta_archivo = input("Ingrese la ruta del archivo .json: ")
    try:
        with open(ruta_archivo, "r") as file:
            data = file.read()
            lexer.input(data)
            while True:
                tok = lexer.token()
                if not tok:
                    break
                print(f"Se ha encontrado el token:'{tok.value}' Del tipo: {tok.type}")
    except FileNotFoundError:
        print(f"No se pudo encontrar el archivo en la ruta: {ruta_archivo}")

def menu():
    while True:
        mostrar_menu()
        try:
            opcion = int(input("Elige una opción (1-3): "))
            if opcion == 1:
                analizar_entrada_manual()
            elif opcion == 2:
                analizar_archivo()
            elif opcion == 3:
                print("Saliendo del programa...")
                break
            else:
                print("Opción no válida. Por favor, elige una opción del 1 al 3.")
        except ValueError:
            print("Entrada no válida. Por favor, introduce un número.")

if __name__ == "__main__":
    menu()