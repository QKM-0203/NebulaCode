/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.cypher;

import com.vesoft.nebula.orm.operator.*;
import com.vesoft.nebula.orm.query.ngql.Column;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * used to splice parameters into query statements.
 *
 * @author Qi Kai Meng
 */
public class QueryBase {

    private static String joinWhere(Map<String, Filter> conMap, int isEdge) {
        ArrayList<String> whereStrings = new ArrayList<>();
        for (String propName : conMap.keySet()) {
            Filter filter = conMap.get(propName);
            if (filter instanceof Logical) {
                whereStrings.add(joinLogical(propName, (Logical) filter, isEdge));
            } else if (filter instanceof Relational) {
                if (isEdge == 0) {
                    whereStrings.add(String.format("v.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                } else if (isEdge == 1) {
                    whereStrings.add(String.format("e.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                } else {
                    whereStrings.add(String.format("%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));

                }
            } else if (filter instanceof UnaryOperation) {
                if (isEdge == 0) {
                    whereStrings.add(String.format("v.%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                } else if (isEdge == 1) {
                    whereStrings.add(String.format("e.%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                } else {
                    whereStrings.add(String.format("%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                }
            }
        }
        return String.join(Lexer.AND, whereStrings);
    }

    public static String judgeAndJoinWhere(Map<String, Filter> conMap, List<String> filterString, int isEdge) {
        StringBuilder result = new StringBuilder();
        if ((conMap != null && !conMap.isEmpty()) || (filterString != null && !filterString.isEmpty())) {
            result.append(Lexer.WHERE);
            if (conMap != null && !conMap.isEmpty()) {
                result.append(joinWhere(conMap, isEdge));
            }
            if (filterString != null && !filterString.isEmpty()) {
                if (conMap != null && !conMap.isEmpty()) {
                    result.append(Lexer.AND);
                }
                result.append(String.join(Lexer.AND, filterString));
            }
        }
        return result.toString();
    }

    private static String joinLogical(String propName, Logical logical, int isEdge) {
        Relational leftRelational = logical.getLeftRelational();
        Relational rightRelational = logical.getRightRelational();
        if (isEdge == 0) {
            return String.format("(%s %s %s)",
                String.format("v.%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("v.%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        } else if (isEdge == 1) {
            return String.format("(%s %s %s)",
                String.format("e.%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("e.%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        } else {
            return String.format("(%s %s %s)",
                String.format("%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        }
    }

    public static String joinAttributeAlias(List<Column> columns) {
        ArrayList<String> result = new ArrayList<>();
        for (Column column : columns) {
            if (column.getAlias() != null) {
                result.add(column.getPropName() + Lexer.AS + column.getAlias());
            } else {
                result.add(column.getPropName());
            }
        }
        return String.join(",", result);
    }

    public static String joinAttribute(List<Column> columns) {
        ArrayList<String> result = new ArrayList<>();
        for (Column column : columns) {
            result.add(column.getPropName());
        }
        return String.join(",", result);
    }

    protected static String joinAggregateFunctionsAlias(List<Column> aggregateFunctions) {
        ArrayList<String> result = new ArrayList<>();
        for (Column aggregateFunction : aggregateFunctions) {
            if (aggregateFunction.getAlias() == null) {
                result.add(String.format("%s(%s)", aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue()));
            } else {
                result.add(String.format("%s(%s)" + Lexer.AS + "%s", aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue(), aggregateFunction.getAlias()));
            }
        }
        return String.join(",", result);
    }
}
