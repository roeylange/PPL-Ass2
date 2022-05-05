import { map } from 'ramda';
import { Exp, isBoolExp, isNumExp, isVarRef, isProgram, Program, isIfExp, isDefineExp, isPrimOp, isProcExp, isAppExp, PrimOp, CExp } from '../imp/L3-ast';
import { isSymbolSExp } from '../imp/L3-value';
import { Result, makeFailure, bind, mapResult, makeOk, safe3, safe2 } from '../shared/result';
import { isCExp, isLetExp, isLetPlusExp, isLitExp, isStrExp } from './L31-ast';

/*
Purpose: Transform L3 AST to JavaScript program string
Signature: l30ToJS(l2AST)
Type: [EXP | Program] => Result<string>
*/
export const l30ToJS = (exp: Exp | Program): Result<string>  => 
    isProgram(exp) ? bind(mapResult(l30ToJS, exp.exps), exps => makeOk(exps.join(";\n"))) :
    isBoolExp(exp) ? makeOk(exp.val.toString()) :
    isNumExp(exp) ? makeOk(exp.val.toString()) :
    isVarRef(exp) ? makeOk(exp.var) :
    isStrExp(exp) ? makeOk(`\"${exp.val}\"`) :
    isLitExp(exp) ? 
        isSymbolSExp(exp.val) ? makeOk(`Symbol.for("${exp.val.val}")`) : makeOk("") :
    isLetExp(exp) ? bind(l30ToJS(exp.body[exp.body.length-1]), body => makeOk("((" + map((p) => p.var.var, exp.bindings).join(",") + ")" + " => "
    + body + ")("+ map((p) => isNumExp(p.val) ? p.val.val : 0, exp.bindings).join(",") + ")")) :
    isDefineExp(exp) ? bind(l30ToJS(exp.val), val => makeOk(`const ${exp.var.var} = ${val}`)) :
    isIfExp(exp) ? safe3((test: string, then: string, alt: string) => makeOk(`(${test} ? ${then} : ${alt})`))
        (l30ToJS(exp.test), l30ToJS(exp.then), l30ToJS(exp.alt)) :
    isProcExp(exp) ? bind(l30ToJS(exp.body[exp.body.length-1]), body => makeOk("((" + map((p) => p.var, exp.args).join(",") + ")" + " => "
      + body + ")")) :
    isAppExp(exp) ?  
        (
         isPrimOp(exp.rator) ? primOpApp2JS(exp.rator, exp.rands) :
         safe2((rator: string, rands: string[]) => makeOk(`${rator}(${rands.join(",")})`))
            (l30ToJS(exp.rator), mapResult(l30ToJS, exp.rands))
         ) :
    isPrimOp(exp) ? makeOk(convertPrimOp(exp.op)) :
    makeFailure("TODO");

const convertPrimOp = (op : string) : string =>
    op === "=" || op === "eq?" || op === "string=?" ? "===" :
    op === "number?" ? "((x) => (typeof(x) === number))" :
    op === "boolean?" ? "((x) => (typeof(x) === boolean))" :
    op === "symbol?" ? "((x) => (typeof (x) === symbol))" :
    op === "string?" ? "((x) => (typeof (X) === string))" :
    op;       //we return +,-,*,/,<,>,and,or as they are. we ignore cons, car, cdr, pair? and list

const primOpApp2JS = (rator : PrimOp, rands : CExp[]) : Result<string> => 
    rator.op === "not" ? bind(l30ToJS(rands[0]), (rand : string) => makeOk("(!" + rand + ")")) :
    rator.op === "number?" || rator.op === "boolean?" || rator.op === "symbol?" || rator.op === "string?" ? bind(l30ToJS(rands[0]), (rand : string) => makeOk(`${convertPrimOp(rator.op)}(${rands[0]})`)) :
    bind(mapResult(l30ToJS,rands), (rands) => makeOk("(" + rands.join(" " + convertPrimOp(rator.op) + " ") + ")"));    