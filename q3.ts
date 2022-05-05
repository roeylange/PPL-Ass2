import {  Exp, LetPlusExp, LetExp, Program, makeLetExp, Binding, CExp, isExp, isLetPlusExp, isProgram, makeProgram, parseL31Exp, isIfExp, makeIfExp, isDefineExp, makeDefineExp, isAtomicExp, isAppExp, makeAppExp, isProcExp, makeProcExp, isLetExp, makeBinding, isLitExp } from "./L31-ast";
import { Result, makeFailure, makeOk, mapResult } from "../shared/result";
import { first, second, rest, allT, isEmpty } from "../shared/list";
import { bind, map, reduce } from "ramda";
import { parse as p } from "../shared/parser";

/*
Purpose: Transform L31 AST to L3 AST
Signature: l31ToL3(l31AST)
Type: [Exp | Program] => Result<Exp | Program>
*/
export const L31ToL3 = (exp: Exp | Program): Result<Exp | Program> =>
    isExp(exp) ? isLetPlusExp(exp) ? makeOk(ConvertPlusExpToLetExp(exp)) : makeOk(exp) :
        makeOk(makeProgram(map(L31ExpToL3, exp.exps)))
        
const L31ExpToL3 = (exp: Exp): Exp =>
    isDefineExp(exp) ? makeDefineExp(exp.var, L31CExpToL3(exp.val)) : L31CExpToL3(exp)

const L31CExpToL3 = (exp: CExp): CExp =>
    isAppExp(exp) ? makeAppExp(L31CExpToL3(exp.rator), map(L31CExpToL3, exp.rands)) :
    isIfExp(exp) ? makeIfExp(L31CExpToL3(exp.test), L31CExpToL3(exp.then), L31CExpToL3(exp.alt)) :
    isProcExp(exp) ? makeProcExp(exp.args, map(L31CExpToL3, exp.body)) :
    isLetExp(exp) ? makeLetExp(reduce((bindings: Binding[], binding: Binding) => bindings.concat([makeBinding(binding.var.var, L31CExpToL3(binding.val))]), [], exp.bindings), map(L31CExpToL3, exp.body)) :
    isLetPlusExp(exp) ? ConvertPlusExpToLetExp(exp) :
    exp
    
const ConvertPlusExpToLetExp = (exp: LetPlusExp): LetExp => CreateLetExp(exp.bindings, exp.body)
    
const CreateLetExp = (bindings: Binding[], body: CExp[]): LetExp =>
    bindings.length === 1 ? makeLetExp(bindings, body) :
    makeLetExp([first(bindings)], [CreateLetExp(rest(bindings), body)]);