import json
import sys



######################### VARIABLES GLOBALES #########################

FILENAME = "file.s"
ROOT_VAR = "xxx" # Lorsque le compilateur crée une variable, celle-ci commence par ROOT_VAR
func = None
label_id = 0



######################### FONCTIONS FICHIER #########################

def read(filename):
    with open(filename) as f:
        term = json.load(f)
        return term
    
def write(filename, content):
    with open(filename, "a") as f:
        f.write(content + "\n")
        return
    
def reset(filename):
    with open(filename, "w") as f:
        f.write("")
        return



######################### GESTION ERREURS #########################

class Compile_exception(BaseException):
    def __init__(self, msg, term):
        pos = (term["start_line"],term["start_char"],term["end_line"],term["end_char"])
        self.msg = msg + (" at positions (%d,%d)-(%d,%d)"%pos)



######################### OPÉRATIONS BINAIRES #########################

def binop_comparaison(term, dest, local_env, jump):
    global label_id
    eval_expr(term["e1"], "%rax", local_env)
    eval_expr(term["e2"], "%rbx", local_env)
    # Comparaison entre %rax et %rbx
    write(FILENAME, f"\tCMP %rbx, %rax")
    write(FILENAME, f"\t{jump} .true{label_id}")
    # Cas False
    write(FILENAME, f"\tMOV $0, {dest}")
    write(FILENAME, f"\tJMP .end{label_id}")
    # Cas True
    write(FILENAME, f".true{label_id}:")
    write(FILENAME, f"\tMOV $1, {dest}")
    # Fin
    write(FILENAME, f".end{label_id}:")
    label_id += 1

def binop(term, dest, local_env):
    non_dest = "%rax" if dest != "%rax" else "%rbx"
    if term["binop"] == "+":
        eval_expr(term["e1"], non_dest, local_env)
        write(FILENAME, f"\tPUSHQ {non_dest}")
        func_dict[func]["rsp"] += 8
        eval_expr(term["e2"], dest, local_env)
        write(FILENAME, f"\tPOPQ {non_dest}")
        func_dict[func]["rsp"] -= 8
        write(FILENAME, f"\tADD {non_dest}, {dest}")
    elif term["binop"] == "-":
        eval_expr(term["e1"], dest, local_env)
        eval_expr(term["e2"], non_dest, local_env)
        write(FILENAME, f"\tSUB {non_dest}, {dest}")
    elif term["binop"] == "*":
        eval_expr(term["e1"], non_dest, local_env)
        eval_expr(term["e2"], dest, local_env)
        write(FILENAME, f"\tIMUL {non_dest}, {dest}")
    elif term["binop"] == "/":
        eval_expr(term["e1"], "%rax", local_env)
        write(FILENAME, "\tCQTO") # sign extend 64-bit signed %rax to 128-bit signed %rdx:%rax (Convert Quad To Octal)
        eval_expr(term["e2"], "%rbx", local_env)
        write(FILENAME, f"\tIDIV %rbx") #division
        if dest != "%rax":
            write(FILENAME, f"\tMOVQ %rax, {dest}")
    elif term["binop"] == "%":
        eval_expr(term["e1"], "%rax", local_env)
        write(FILENAME, "\tCQTO") # sign extend 64-bit signed %rax to 128-bit signed %rdx:%rax (Convert Quad To Octal)
        eval_expr(term["e2"], "%rbx", local_env)
        write(FILENAME, f"\tIDIV %rbx") #division
        if dest != "%rdx":
            write(FILENAME, f"\tMOVQ %rdx, {dest}")
    elif term["binop"] == "==":
        binop_comparaison(term, dest, local_env, "JE")
    elif term["binop"] == "!=":
        binop_comparaison(term, dest, local_env, "JNE")
    elif term["binop"] == ">=":
        binop_comparaison(term, dest, local_env, "JGE")
    elif term["binop"] == ">":
        binop_comparaison(term, dest, local_env, "JG")
    elif term["binop"] == "<=":
        binop_comparaison(term, dest, local_env, "JLE")
    elif term["binop"] == "<":
        binop_comparaison(term, dest, local_env, "JL")
    else:
        assert 0, "Opérateur inconnu ou && ou ||"



######################### SIMPLIFICATION DES EXPRESSIONS #########################

def temp_simplifier_dico(arg):
    # Fonction de débuggage
    if type(arg) == dict:
        new_dico = {}
        for key, value in arg.items():
            if key not in ['start_line', 'start_char', 'end_line', 'end_char']:
                new_dico[key] = temp_simplifier_dico(value)
        return new_dico
    elif type(arg) == list:
        new_list = []
        for value in arg:
            new_list.append(temp_simplifier_dico(value))
        return new_list
    else:
        return arg
    
def afficher_dico(arg):
    # Fonction de débuggage
    for dico in temp_simplifier_dico(arg):
        print(dico)
        
def get_type_size(dico):
    if dico["type"] == "int":
        if dico["pointer"] == 0:
            return 8
        else:
            return 8
    elif dico["type"] == "char":
        if dico["pointer"] == 0:
            return 8
        else:
            return 8
    else:
        raise Compile_exception("Type inconnu", dico)

def get_dico_pos(dico):
    return {'start_line': dico['start_line'], 'start_char': dico['start_char'], 'end_line': dico['end_line'], 'end_char': dico['end_char']}

def simplification_aux(dico,nom_var):
    # La barre verticale concatène des dictionnaires
    dico_pos = get_dico_pos(dico)
    match dico['type']:
        case 'cst_int':
            vardef={'action': 'vardef', 'ctype':{"type":"int","pointer":0} | dico_pos, 'name': nom_var} | dico_pos
            varset={'action': 'varset', "pointer":0, 'name': nom_var, 'expr': {'type': 'cst_int', 'value': dico['value']} | dico_pos} | dico_pos
            return [vardef,varset]
        case 'binop':
            e1 = {'type': 'var', 'pointer':0, "dimensions": [], 'name':nom_var+'a'} | dico_pos if dico['e1']['type']!='var' else dico['e1']
            e2 = {'type': 'var', 'pointer':0, "dimensions": [], 'name':nom_var+'b'} | dico_pos if dico['e2']['type']!='var' else dico['e2']
            vardef={'action': 'vardef', 'ctype':{"type":"int","pointer":0} | dico_pos, 'name': nom_var} | dico_pos
            varset={'action': 'varset', 'pointer':0, 'name': nom_var, 'expr': {'type': 'binop', 'binop': dico['binop'], 'e1':e1 | dico_pos, 'e2':e2 | dico_pos} | dico_pos} | dico_pos
            left, right = [], []
            if dico['e1']['type'] != 'var':
                left = simplification_aux(dico['e1'],nom_var+'a')
            if dico['e2']['type'] != 'var':
                right = simplification_aux(dico['e2'],nom_var+'b')
            if dico['binop'] == "&&" or dico['binop'] == "||":
                varset_right={'action': 'varset', 'pointer':0, 'name': nom_var, 'expr': e2} | dico_pos
                varset_left={'action': 'varset', 'pointer':0, 'name': nom_var, 'expr': e1} | dico_pos
                if dico['binop'] == "&&":
                    return [vardef] + left + [{"action": "ifelse", "expr": e1, "body1":right + [varset_right], "body2":[varset_left]} | dico_pos]
                else:
                    return [vardef] + left + [{"action": "ifelse", "expr": e1, "body1":[varset_left], "body2":right + [varset_right]} | dico_pos]
            else:
                return [vardef] + left + right + [varset]
        case 'application':
            l = []
            vardef={'action': 'vardef', 'ctype':{"type":"int","pointer":0} | dico_pos, 'name': nom_var} | dico_pos
            l.append(vardef)
            liste_args = []
            for i, arg in enumerate(dico['argvalue']):
                if arg['type']!='var':
                    l += simplification_aux(arg, nom_var+"a"+str(i))
                    liste_args.append({'type': 'var', "pointer":0, "dimensions": [], 'name':nom_var+"a"+str(i)} | dico_pos)
                else:
                    liste_args.append(arg)
            varset={'action': 'varset', 'name': nom_var, "pointer":0, 'expr': {'type': 'application', 'function': dico['function'], 'argvalue':liste_args} | dico_pos} | dico_pos
            l.append(varset)
            return l
        case "address":
            l = []
            vardef={'action': 'vardef', 'ctype':{"type":"int","pointer":0} | dico_pos, 'name': nom_var} | dico_pos
            l.append(vardef)
            varset={'action': 'varset', "pointer":0, 'name': nom_var, 'expr': dico | dico_pos} | dico_pos
            l.append(varset)
            return l
        case "sizeof":
            l = []
            vardef={'action': 'vardef', 'ctype':{"type":"int","pointer":0} | dico_pos, 'name': nom_var} | dico_pos
            l.append(vardef)
            varset={'action': 'varset', "pointer":0, 'name': nom_var, 'expr': {"type": "cst_int", "value": get_type_size(dico["ctype"])} | dico_pos} | dico_pos
            l.append(varset)
            return l
        case _: 
            assert 0, "Erreur dans simplification_aux"

def simplification(body, cond=None): #Fonction qui va simplifier les expressions du body de la fonction f
    body_replacement=[]
    for i, statement in enumerate(body):
        match statement["action"]:
            case "empty":
                pass # Les statements vides sont ignorés
            case "expr":
                if statement["expr"]["type"]=="application":
                    # Seules les fonctions peuvent avoir un effet sur l'exécution dans ce cas
                    l = []
                    liste_args = []
                    for j, arg in enumerate(statement["expr"]['argvalue']):
                        if arg['type']!='var':
                            l += simplification_aux(arg, ROOT_VAR+str(i)+"arg"+str(j))
                            liste_args.append({'type': 'var', 'pointer': 0,  "dimensions": [], 'name': ROOT_VAR+str(i)+"arg"+str(j)} | get_dico_pos(statement["expr"]))
                        else:
                            liste_args.append(arg)
                    statement['expr']['argvalue'] = liste_args
                    body_replacement += l + [statement]
            case "varset":
                if statement["expr"]["type"] == "application":
                    # Ce cas évite de créer une variable intermédiaire inutile
                    l = []
                    liste_args = []
                    for j, arg in enumerate(statement["expr"]['argvalue']):
                        if arg['type']!='var':
                            l += simplification_aux(arg, ROOT_VAR+str(i)+"arg"+str(j))
                            liste_args.append({'type': 'var', 'pointer': 0,  "dimensions": [], 'name': ROOT_VAR+str(i)+"arg"+str(j)} | get_dico_pos(statement["expr"]))
                        else:
                            liste_args.append(arg)
                    statement['expr']['argvalue'] = liste_args
                    body_replacement += l
                elif statement["expr"]["type"] in ["binop", "sizeof"]:
                    temp=simplification_aux(statement['expr'],ROOT_VAR+str(i))
                    statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                    body_replacement += temp
                body_replacement+=[statement]
            case "while":
                if statement["expr"]["type"]!='var':
                    temp=simplification_aux(statement['expr'],'whilex'+str(i))
                    statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':'whilex'+str(i)} | get_dico_pos(statement)
                    body_replacement += temp
                    statement['body'] = simplification(statement['body'], temp[1:])
                    statement['body'] += temp[1:] # on remet à jour la condition de boucle
                else:
                    statement['body'] = simplification(statement['body'], [])
                body_replacement+=[statement]
            case "if":
                statement['body'] = simplification(statement['body'], cond)
                if statement["expr"]["type"]!='var':
                    temp=simplification_aux(statement['expr'],ROOT_VAR+str(i))
                    statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                    body_replacement += temp
                body_replacement+=[statement]
            case "ifelse":
                statement['body1'] = simplification(statement['body1'], cond)
                statement['body2'] = simplification(statement['body2'], cond)
                if statement["expr"]["type"]!='var':
                    temp=simplification_aux(statement['expr'],ROOT_VAR+str(i))
                    statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                    body_replacement += temp
                body_replacement+=[statement]
            case "return":
                if statement["expr"]["type"]!='var':
                    if statement["expr"]["type"] == "application":
                        # Ce cas évite de créer une variable intermédiaire inutile
                        l = []
                        liste_args = []
                        for j, arg in enumerate(statement["expr"]['argvalue']):
                            if arg['type']!='var':
                                l += simplification_aux(arg, ROOT_VAR+str(i)+"arg"+str(j))
                                liste_args.append({'type': 'var', 'pointer': 0,  "dimensions": [], 'name': ROOT_VAR+str(i)+"arg"+str(j)} | get_dico_pos(statement["expr"]))
                            else:
                                liste_args.append(arg)
                        statement['expr']['argvalue'] = liste_args
                        body_replacement += l
                    elif statement["expr"]["type"] in ["binop", "sizeof"]:
                        temp=simplification_aux(statement['expr'],ROOT_VAR+str(i))
                        statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                        body_replacement += temp
                body_replacement+=[statement]
                break # On ignore les statements après le return
            case "continue":
                body_replacement += cond
                body_replacement+=[statement]
            case "break":
                body_replacement += cond
                body_replacement+=[statement]
            case "tabset":
                for j, dim in enumerate(statement["dimensions"]):
                    if dim["type"] != "var":
                        temp = simplification_aux(dim, ROOT_VAR+str(i)+"dim"+str(j))
                        statement["dimensions"][j] = {'type':'var','pointer':0, "dimensions": [],'name': ROOT_VAR+str(i)+"dim"+str(j)} | get_dico_pos(statement)
                        body_replacement += temp
                if statement["expr"]["type"] != "var":
                    temp = simplification_aux(statement["expr"], ROOT_VAR+str(i))
                    statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                    body_replacement += temp
                body_replacement += [statement]
            case "malloc":
                if statement["expr"]["type"]!='var':
                    if statement["expr"]["type"] == "application":
                        # Ce cas évite de créer une variable intermédiaire inutile
                        l = []
                        liste_args = []
                        for j, arg in enumerate(statement["expr"]['argvalue']):
                            if arg['type']!='var':
                                l += simplification_aux(arg, ROOT_VAR+str(i)+"arg"+str(j))
                                liste_args.append({'type': 'var', 'pointer': 0,  "dimensions": [], 'name': ROOT_VAR+str(i)+"arg"+str(j)} | get_dico_pos(statement["expr"]))
                            else:
                                liste_args.append(arg)
                        statement['expr']['argvalue'] = liste_args
                        body_replacement += l
                    elif statement["expr"]["type"] in ["binop", "sizeof"]:
                        temp=simplification_aux(statement['expr'],ROOT_VAR+str(i))
                        statement['expr']={'type':'var','pointer':0,"dimensions": [],'name':ROOT_VAR+str(i)} | get_dico_pos(statement)
                        body_replacement += temp
                body_replacement+=[statement]
            case _:
                body_replacement+=[statement]
    return body_replacement



######################### GESTION DES FONCTIONS #########################

def create_func(term):
    global func
    func = term['name']
    write(FILENAME, f"\n{func}:")
    write(FILENAME, "\tPUSH %rbp") # Pas de func_dict[func]['rsp'] += 8 car %rbp est mise à %rsp après
    write(FILENAME, "\tMOVQ %rsp, %rbp\n")

    """   GESTION DES ARGUMENTS (stockés dans la pile avant l'appel)  """
    env = {}
    for i, vardef in enumerate(term['arg']):
        if func == "main":
            # Les arguments de main ne se trouvent pas dans la pile, on les crée
            env = eval_stmt(vardef, env, None, 0) #fait un PUSHQ $0!
        else:
            # Les arguments ont déjà été poussés dans la pile, on les récupère
            if vardef["action"] == "vardef":
                env[vardef["name"]] = {"offset": -8*(i+2), "dimensions": 0} # +2 car CALL met %rip+8 dans 8(%rbp)
            elif vardef["action"] == "tabdef":
                env[vardef["name"]] = {"offset": -8*(i+2), "dimensions": vardef["dimensions"]} # +2 car CALL met %rip+8 dans 8(%rbp)
            else:
                raise Compile_exception("Argument de fonction invalide", term)

    has_return = False
    if func == "print_int":
        if func_dict[func]['rsp'] % 16 == 8:
            write(FILENAME, f"\tPUSHQ $0")
            func_dict[func]['rsp'] += 8
        write(FILENAME, f"\tMOVQ -{env[term['arg'][0]['name']]['offset']}(%rbp), %rsi")
        write(FILENAME, "\tLEAQ print_fmt(%rip), %rdi")
        write(FILENAME, f"\tXOR %rax, %rax")
        write(FILENAME, "\tCALL printf")
        write(FILENAME, f"\tMOVQ $0, %r8") # On pourra renvoyer le nombre de caractères print
    elif func == "read_int":
        write(FILENAME, f"\tPUSHQ $0")
        func_dict[func]['rsp'] += 8
        if func_dict[func]['rsp'] % 16 == 8:
            write(FILENAME, f"\tPUSHQ $0")
            func_dict[func]['rsp'] += 8
        write(FILENAME, "\tLEAQ (%rsp), %rsi")
        write(FILENAME, "\tLEAQ scanf_fmt(%rip), %rdi")
        write(FILENAME, "\tXOR %rax, %rax")
        write(FILENAME, "\tCALL scanf")
        write(FILENAME, "\tPOPQ %r8")
    else:
        for elem in func_dict[func]["body"]:
            env = eval_stmt(elem, env, None, 0)
            if elem["action"] == "return":
                # On s'arrête au premier return car la suite ne sera jamais exécutée
                has_return = True
                break
    if not has_return:
        # On rajoute un return même dans le cas où il n'y en avait pas dans le c
        write(FILENAME, f"\tMOVQ %rbp, %rsp")
        write(FILENAME, f"\tPOPQ %rbp")
        write(FILENAME, f"\tXOR %rax, %rax")
        write(FILENAME, "\tRET")



######################### FONCTIONS EVAL #########################

def eval_expr(term, dest, local_env):
    """
    Entrée : dictionnaire de l'expression à traiter, le registre dans lequel mettre le résultat du traitement,
             dictionnaire de l'environnement local (le nom des variables associés aux adresses où leurs valeurs sont stockées)
    Sortie : le nom de la variable contenant la valeur de l'expression
    """
    global func
    match term["type"]:
        case "application":
            if term["function"] == "main":
                raise Compile_exception("On ne peut pas appeler le main", term)
            else:
                if term["function"] not in func_dict.keys():
                    raise Compile_exception("La fonction n'existe pas !", term)
                n = len(func_dict[term["function"]]["arg"])
                for i in range(n-1, -1, -1):
                    eval_expr(term["argvalue"][i], "%rax", local_env)
                    write(FILENAME, f"\tPUSHQ %rax") # On en modifie pas func_dict[func]['rsp'] car %rsp est rétabli à la fin
                write(FILENAME, f"\tCALL {term['function']}")
                write(FILENAME, f"\tADDQ ${n*8}, %rsp") # On libère l'espace pris par les paramètres
                if dest != "%r8":
                    write(FILENAME, f"\tMOVQ %r8, {dest}")
        case "cst_int":
            write(FILENAME, f"\tMOVQ ${term['value']}, {dest}")
        case "binop":
            binop(term, dest, local_env)
        case "var":
            if "pointer" not in term.keys():
                raise Compile_exception("Pas de pointeur renseigné", term)
            if term["dimensions"] != []:
                non_dest = "%rax" if dest != "%rax" else "%rbx"
                if term["name"] in local_env.keys():
                    # Récupération de l'adresse à écrire dans %r9
                    write(FILENAME, "\tMOVQ $0, %r9")
                    for i, dim in enumerate(term['dimensions']):
                        eval_expr(dim,"%r8",local_env) # Je récupère l'indice
                        # Tableau linéarisé
                        offset = 1
                        for j in range(i+1, len(term['dimensions'])):
                            offset *= local_env[term["name"]]["dimensions"][j]
                        write(FILENAME, f"\tIMUL ${offset}, %r8")
                        write(FILENAME, "\tADDQ %r8, %r9")

                    write(FILENAME, "\tIMUL $8, %r9")
                    write(FILENAME, f"\tMOVQ -{local_env[term['name']]['offset']}(%rbp), {non_dest}")
                    write(FILENAME, f"\tADD %r9, {non_dest}")
                    write(FILENAME, f"\tMOVQ ({non_dest}), {dest}")
                elif term["name"] in global_var.keys():
                    # Récupération de l'adresse à écrire dans %r9
                    write(FILENAME, "\tMOVQ $0, %r9")
                    for i, dim in enumerate(term['dimensions']):
                        eval_expr(dim,"%r8",local_env) # Je récupère l'indice
                        # Tableau linéarisé
                        offset = 1
                        for j in range(i+1, len(term['dimensions'])):
                            offset *= global_var[term["name"]]["dimensions"][j]
                        write(FILENAME, f"\tIMUL ${offset}, %r8")
                        write(FILENAME, "\tADDQ %r8, %r9")

                    write(FILENAME, "\tIMUL $8, %r9")
                    write(FILENAME, f"\tLEAQ {term['name']}(%rip), {non_dest}")
                    write(FILENAME, f"\tADD %r9, {non_dest}")
                    write(FILENAME, f"\tMOVQ ({non_dest}), {dest}")
                else:
                    raise Compile_exception("La variable n'existe pas !", term)
            elif term["name"] in local_env.keys():
                write(FILENAME, f"\tMOVQ -{local_env[term['name']]['offset']}(%rbp), {dest}")
                non_dest = "%rax" if dest != "%rax" else "%rbx"
                for i in range(term["pointer"]):
                    write(FILENAME, f"\tMOVQ ({dest}), {non_dest}")
                    write(FILENAME, f"\tMOVQ {non_dest}, {dest}")
            elif term["name"] in global_var.keys():
                write(FILENAME, f"\tMOVQ {term['name']}(%rip), {dest}")
            else:
                raise Compile_exception("La variable n'existe pas !", term)
        case "address":
            if term['var'] in local_env.keys():
                write(FILENAME, f"\tLEAQ -{local_env[term['var']]['offset']}(%rbp), {dest}")
            elif term['var'] in global_var.keys():
                write(FILENAME, f"\tLEAQ {term['var']}(%rip), {dest}")
        case _:
            assert 0, "Expression non reconnue"

def eval_stmt(term, local_env, id_boucle, rsp_boucle):
    """
    term       -> dico décrivant le statement
    local_env  -> dico associant les noms de variables à leur position dans la pile depuis %rbp
    id_boucle  -> label_id associée à la boucle parent direct si elle existe, sinon None
    rsp_boucle -> décalage de rsp depuis le début de la boucle parent direct
    """
    global func
    global label_id
    match term["action"]:
        case "expr":
            if term["expr"]["type"] == "application":
                # Seul cas où le statement expr; peut avoir un effet
                eval_expr(term["expr"], "%r8", local_env)
        case "vardef":
            write(FILENAME, "\tPUSHQ $0")
            func_dict[func]["rsp"] += 8
            local_env[term["name"]] = {"offset": func_dict[func]["rsp"], "dimensions": []}
        case "return":
            if term["expr"]["type"] == "application" and term["expr"]["function"] == func:
                # Cas de la récursivité terminale
                for i, var in enumerate(term["expr"]["argvalue"]):
                    write(FILENAME, f"\tMOVQ -{local_env[var['name']]['offset']}(%rbp), %rax")
                    write(FILENAME, f"\tMOVQ %rax, {8*(i+2)}(%rbp)")
                write(FILENAME, f"\tMOVQ %rbp, %rsp")
                write(FILENAME, f"\tPOPQ %rbp")
                write(FILENAME, f"\tXOR %rax, %rax")
                write(FILENAME, f"\tJMP {func}")
            else:
                eval_expr(term["expr"], "%r8", local_env)
                write(FILENAME, f"\tMOVQ %rbp, %rsp")
                write(FILENAME, f"\tPOPQ %rbp")
                write(FILENAME, f"\tXOR %rax, %rax")
                write(FILENAME, "\tRET")
        case "varset":
            if term["name"] in local_env.keys():
                if term["expr"]["type"] == "cst_int": #option hardcodée pour éviter que le code boucle à l'infini
                    write(FILENAME, f"\tLEAQ -{local_env[term['name']]['offset']}(%rbp), %rax")
                    for _ in range(term["pointer"]):
                        write(FILENAME, "\tMOVQ (%rax), %rbx")
                        write(FILENAME, "\tMOVQ %rbx, %rax")
                    write(FILENAME, f"\tMOVQ ${term['expr']['value']}, (%rax)")
                else:
                    eval_expr(term["expr"], "%r8", local_env)
                    write(FILENAME, f"\tLEAQ -{local_env[term['name']]['offset']}(%rbp), %rax")
                    for _ in range(term["pointer"]):
                        write(FILENAME, "\tMOVQ (%rax), %rbx")
                        write(FILENAME, "\tMOVQ %rbx, %rax")
                    write(FILENAME, "\tMOVQ %r8, (%rax)")
            elif term["name"] in global_var.keys():
                if term["expr"]["type"] == "cst_int": #option hardcodée pour éviter que le code boucle à l'infini
                    write(FILENAME, f"\tLEAQ {term['name']}(%rip), %rax")
                    for _ in range(term["pointer"]):
                        write(FILENAME, "\tMOVQ (%rax), %rbx")
                        write(FILENAME, "\tMOVQ %rbx, %rax")
                    write(FILENAME, f"\tMOVQ ${term['expr']['value']}, (%rax)")
                else:
                    eval_expr(term["expr"], "%r8", local_env)
                    write(FILENAME, f"\tLEAQ {term['name']}(%rip), %rax")
                    for _ in range(term["pointer"]):
                        write(FILENAME, "\tMOVQ (%rax), %rbx")
                        write(FILENAME, "\tMOVQ %rbx, %rax")
                    write(FILENAME, "\tMOVQ %r8, (%rax)")
            else:
                raise Compile_exception("La variable doit être déclarée au préalable !", term)
        case "tabintdef":
            # Calcul de l'espace à allouer (les dimensions ne comportent que des cst_int)
            space = term["dimensions"][0]["value"]
            for i, dim in enumerate(term["dimensions"]):
                if i > 0:
                    space *= dim["value"]
            space *= 8 # Cas des int (SUR QUE C'EST NECESSAIRE VU QU'ON IMUL 8 DANS MALLOC ?)

            # Allocation de la mémoire
            local_env = eval_stmt({"action": "malloc", "name": term["name"], "expr": {"type": "cst_int", "value": space}}, local_env, id_boucle, rsp_boucle)
            local_env[term["name"]]["dimensions"] = [dim["value"] for dim in term["dimensions"]]
        case "tabset":
            if term["name"] in local_env.keys():
                if term["expr"]["type"] == "var":
                    write(FILENAME, f"\tMOVQ -{local_env[term['name']]['offset']}(%rbp), %r8")
                    for _ in range(term["pointer"]):
                        write(FILENAME, "\tMOVQ (%r8), %r9")
                        write(FILENAME, "\tMOVQ %r9, %r8")
                    # Récupération de l'offset de l'adresse à écrire dans %rbx
                    write(FILENAME, "\tMOVQ $0, %rbx")
                    for i, dim in enumerate(term['dimensions']):
                        eval_expr(dim,"%rax",local_env) # Je récupère l'indice
                        # Tableau linéarisé
                        offset = 1
                        for j in range(i+1, len(term['dimensions'])):
                            offset *= local_env[term["name"]]["dimensions"][j]
                        write(FILENAME, f"\tIMUL ${offset}, %rax")
                        write(FILENAME, f"\tADDQ %rax, %rbx")
                    write(FILENAME, "\tIMUL $8, %rbx") # Je le multiplie par 8 pour obtenir le décalage
                    write(FILENAME, "\tADD %rbx, %r8") # J'obtiens l'adresse de la case à modifier en ajoutant le décalage
                    eval_expr(term["expr"], "%rax", local_env) # Je récupère la valeur à insérer
                    write(FILENAME, "\tMOVQ %rax, (%r8)") # Je modifie la valeur à cette adresse
                else:
                    raise Compile_exception("Simplification mal exécutée : la right_value devrait être une variable", term)
            elif term["name"] in global_var.keys():
                if term["expr"]["type"] == "var":
                    if term["pointer"] == 0:
                        # Récupération de l'offset de l'adresse à écrire dans %rbx
                        write(FILENAME, "\tMOVQ $0, %rbx")
                        for i, dim in enumerate(term['dimensions']):
                            eval_expr(dim,"%rax",local_env) # Je récupère l'indice
                            # Tableau linéarisé
                            offset = 1
                            for j in range(i+1, len(term['dimensions'])):
                                offset *= global_var[term["name"]]["dimensions"][j]
                            write(FILENAME, f"\tIMUL ${offset}, %rax")
                            write(FILENAME, "\tADDQ %rax, %rbx")
                        write(FILENAME, "\tIMUL $8, %rbx") # Je le multiplie par 8 pour obtenir le décalage
                        write(FILENAME, f"\tLEAQ {term['name']}(%rip), %r8") # Je récupère l'adresse du tableau
                        write(FILENAME, "\tADD %rbx, %r8") # J'obtiens l'adresse de la case à modifier en ajoutant le décalage
                        eval_expr(term["expr"], "%rax", local_env) # Je récupère la valeur à insérer
                        write(FILENAME, f"\tMOVQ %rax, (%r8)") # Je modifie la valeur à cette adresse
                    else:
                        assert 0, "set >= 1 non implémenté"
                else:
                    raise Compile_exception("Simplification mal exécutée : la right_value devrait être une variable", term)
            else:
                raise Compile_exception("La variable doit être déclarée au préalable !", term)
        case "if":
            if term['expr']["name"] in local_env.keys():
                write(FILENAME, f"\tMOVQ -{local_env[term['expr']['name']]['offset']}(%rbp), %rax")
            elif term['expr']["name"] in global_var.keys():
                write(FILENAME, f"\tMOVQ {term['expr']['name']}(%rip), %rax")
            write(FILENAME, "\tMOVQ $0, %rbx")
            write(FILENAME, "\tCMP %rax, %rbx")
            current_label_id = label_id
            label_id += 1
            write(FILENAME, f"\tJNE .if{current_label_id}")
            write(FILENAME, f"\tJMP .continue{current_label_id}")
            write(FILENAME, f".if{current_label_id}:")
            old_rsp = func_dict[func]["rsp"]
            has_return = False
            for elem in term["body"]:
                local_env = eval_stmt(elem, local_env, id_boucle, rsp_boucle)
                if elem["action"] == "return":
                    # On s'arrête au premier return car la suite ne sera jamais exécutée
                    has_return = True
                    break
            if not has_return:
                write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - old_rsp}, %rsp") #on remet la pile à l'état d'avant le if
            func_dict[func]['rsp'] = old_rsp
            write(FILENAME, f".continue{current_label_id}:")
        case "ifelse":
            if term['expr']["name"] in local_env.keys():
                write(FILENAME, f"\tMOVQ -{local_env[term['expr']['name']]['offset']}(%rbp), %rax")
            elif term['expr']["name"] in global_var.keys():
                write(FILENAME, f"\tMOVQ {term['expr']['name']}(%rip), %rax")
            write(FILENAME, "\tMOVQ $0, %rbx")
            write(FILENAME, "\tCMP %rax, %rbx")
            current_label_id = label_id
            label_id += 1
            write(FILENAME, f"\tJNE .if{current_label_id}")

            """   BLOC ELSE   """
            old_rsp = func_dict[func]["rsp"]
            has_return = False
            for elem in term["body2"]:
                local_env = eval_stmt(elem, local_env, id_boucle, rsp_boucle)
                if elem["action"] == "return":
                    # On s'arrête au premier return car la suite ne sera jamais exécutée
                    has_return = True
                    break
            if not has_return:
                write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - old_rsp}, %rsp") #on remet la pile à l'état d'avant le if
                write(FILENAME, f"\tJMP .continue{current_label_id}")

            """   BLOC IF   """
            write(FILENAME, f".if{current_label_id}:")
            func_dict[func]['rsp'] = old_rsp
            has_return = False
            for elem in term["body1"]:
                local_env = eval_stmt(elem, local_env, id_boucle, rsp_boucle)
                if elem["action"] == "return":
                    # On s'arrête au premier return car la suite ne sera jamais exécutée
                    has_return = True
                    break
            if not has_return:
                write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - old_rsp}, %rsp") #on remet la pile à l'état d'avant le if
            func_dict[func]['rsp'] = old_rsp
            write(FILENAME, f".continue{current_label_id}:")
        case "while":
            # La condition de boucle est recalculée avant de revenir au début de la boucle
            current_label_id = label_id
            label_id += 1
            write(FILENAME, f".cond{current_label_id}:")
            if term['expr']["name"] in local_env.keys():
                write(FILENAME, f"\tMOVQ -{local_env[term['expr']['name']]['offset']}(%rbp), %rax")
            elif term['expr']["name"] in global_var.keys():
                write(FILENAME, f"\tMOVQ {term['expr']['name']}(%rip), %rax")
            else:
                assert 0, "Condition de boucle non reconnue"
            write(FILENAME, "\tMOVQ $0, %rbx")
            write(FILENAME, "\tCMP %rax, %rbx")
            write(FILENAME, f"\tJE .end{current_label_id}")

            rsp_boucle = func_dict[func]["rsp"]
            has_return = False
            for elem in term["body"]:
                local_env = eval_stmt(elem, local_env, current_label_id, rsp_boucle)
                if elem["action"] == "return":
                    # On s'arrête au premier return car la suite ne sera jamais exécutée
                    has_return = True
                    break
            if not has_return:
                write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - rsp_boucle}, %rsp") #on remet la pile à l'état d'avant le while
                write(FILENAME, f"\tJMP .cond{current_label_id}") #condition de boucle
            write(FILENAME, f".end{current_label_id}:")
            func_dict[func]['rsp'] = rsp_boucle
        case "break":
            write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - rsp_boucle}, %rsp") #on remet la pile à l'état d'avant le while
            write(FILENAME, f"\tJMP .end{id_boucle}")
        case "continue":
            write(FILENAME, f"\tADDQ ${func_dict[func]['rsp'] - rsp_boucle}, %rsp") # On retient le décalage de %rsp depuis le début de la boucle
            write(FILENAME, f"\tJMP .cond{id_boucle}")
        case "malloc":
            if func_dict[func]['rsp'] % 16 == 8:
                write(FILENAME, f"\tPUSHQ $0")
                func_dict[func]['rsp'] += 8
            eval_expr(term["expr"], "%rdi", local_env)
            write(FILENAME, f"\tCALL malloc")
            write(FILENAME, "\tPUSHQ %rax")
            func_dict[func]["rsp"] += 8
            local_env[term["name"]] = {"offset": func_dict[func]["rsp"], "dimensions": []}
            write(FILENAME, f"\tMOVQ $0, %rdi")
        case _:
            assert 0, "Problème eval_stmt"
    return local_env

def eval_program(program):
    global global_var
    global_var = {}
    global func_dict
    func_dict = {}
    write(FILENAME, "\t.data")
    write(FILENAME, "print_fmt:\n\t.string \"%d\\n\"")
    write(FILENAME, "scanf_fmt:\n\t.string \"%d\"")
    # Ajout des variables globales
    for elem in program:
        if elem["action"] == "gvardef":
            global_var[elem["name"]] = {"pointer":elem["ctype"]["pointer"],"dimensions":[dim["value"] for dim in elem["dimensions"]]} | get_dico_pos(elem)
            d = len(elem["dimensions"])
            if d == 0:
                write(FILENAME, f"{elem['name']}:\n\t.quad 0")
            else:
                space = 1
                for i, dim in enumerate(elem["dimensions"]):
                    if dim["type"] != "cst_int":
                        raise Compile_exception("La taille d'un tableau global doit être donné en int", elem)
                    else:
                        space *= dim["value"]
                write(FILENAME, f"{elem['name']}:\n\t.space {8*space}")
    write(FILENAME, "\t.text")
    write(FILENAME, "\t.globl main")
    # Ajout de print_int et read_int
    dico_pos = {"start_line":-1,"start_char":0,"end_line":-1,"end_char":0}
    program = [
        {"action": "fundef", "name": "print_int", "arg": [{"action":"vardef", "ctype":{"type":"int","pointer":0} | dico_pos, "name":"x"} | dico_pos], "body": []} | dico_pos,
        {"action": "fundef", "name": "read_int", "arg": [], "body": []} | dico_pos
    ] + program
    # Ajout des autres fonctions
    for elem in program:
        if elem["action"] == "fundef":
            func_dict[elem["name"]] = {"arg":elem["arg"],"body":simplification(elem["body"]),"rsp":0} | get_dico_pos(elem)
            create_func(elem)
    # Test de la bonne présence de main
    if "main" not in func_dict:
        raise Compile_exception("La fonction main est manquante.", {"start_line":-1,"start_char":0,"end_line":-1,"end_char":0})
    else:
        write(FILENAME, f"\n\t.section .note.GNU-stack")



######################### DÉMARRAGE DE LA COMPILATION #########################

if __name__ == "__main__":
    assert(len(sys.argv) == 2)
    reset(FILENAME)
    try:
        eval_program(read(sys.argv[-1]))
    except Compile_exception as i:
        print(i.msg)
        exit(-1)
