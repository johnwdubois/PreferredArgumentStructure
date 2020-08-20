# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from collections import Counter, defaultdict, namedtuple
import glob
import pandas as pd
import pickle
from conllu import parse_tree, parse

# summarize verb_dep args
CORE_TAGS = ["nsubj",
"obj",
"iobj",
"csubj",
"ccomp",
"xcomp"]

# get all verbs and dependents
# from UD 2.5, with Richard Futrell cliqs tool for
# processing
fns = glob.glob("UD-2.5/ud-treebanks-v2.5/*/all.c*")
fns = [i for i in fns if "UD_" in i]


def get_info(sent):
    p = parse_tree(sent)
    try:
        stack = [p[0]]
    except:
        return None
    a = []
    while len(stack) > 0:
        cur = stack.pop(0)
        curargs = []
        for i in cur.children:
            if (cur.token['upostag'] in ["VERB"]):            
                deps = ""
                deps = "-".join([j.token["deprel"] for j in i.children if j.token["deprel"] != "punct"])
                curargs += [{"pos": i.token['upostag'], "dep": i.token["deprel"], "deps": deps}]
            stack += [i]
        if cur.token['upostag'] in ["VERB"]:
            a += [{"pos":cur.token["upostag"],
               "deprel":cur.token["deprel"],
               "curargs": curargs,
                  "token": cur.token}]
    return a

def classify_subj(arg):
    if "csubj" in arg["deps"]:
        return "comp"
    if arg["deps"] == "":
        return arg["pos"]
    elif arg["deps"] == "det":
        return arg["pos"] + "-det"
    else:
        return arg["pos"] + "-modified"
    
def classify_obj(arg):
    if "xcomp" in arg["deps"] or "ccomp" in arg["dep"]:
        return "comp"
    if arg["deps"] == "":
        return arg["pos"]
    elif arg["deps"] == "det":
        return arg["pos"] + "-det"
    else:
        return arg["pos"] + "-modified"

def classify_verb_arg(a, lang):
    d = {"lang": lang,
         "deprel": a["deprel"],
         "pos": a["pos"],
         "feats": a["token"]["feats"],
         "has_subj": any(["subj" in i["dep"] for i in a["curargs"]]),
         "has_obj": any(["obj" in i["dep"] for i in a["curargs"]]),
         "has_comp": any(["ccomp" in i["dep"] or "xcomp" in i["dep"] for i in a["curargs"]]),
         "subj_type": "_".join([classify_subj(i) for i in a["curargs"] if "subj" in i["dep"]]),
         "obj_type": "_".join([classify_obj(i) for i in a["curargs"] if "obj" in i["dep"]])
        }
    return d

# process all sentences, from all corpora
#Mac version: langs = {fn.split("/")[-2]: [get_info(sent) for sent in 
langs = {fn.split("\\")[-2]: [get_info(sent) for sent in 
                            open(fn, encoding="utf-8").read().split("\n\n")] for fn in fns}

langs

lang_d = defaultdict(list)
for lang in langs:
    for sent in langs[lang]:
        if sent is not None:
            for verb in sent:
                lang_d[lang] += [classify_verb_arg(verb, lang)]
df = pd.concat([pd.DataFrame(lang_d[i]) for i in lang_d])

df.loc[df.deprel == "root"].to_csv("args_newway_202008.csv")            