/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.util;

import com.vesoft.nebula.orm.ngql.AttributeColumn;
import com.vesoft.nebula.orm.ngql.Encoding;
import com.vesoft.nebula.orm.ngql.FunctionColumn;
import com.vesoft.nebula.orm.operator.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * used to splice parameters into query statements
 */
public class QueryBase {

    private static String joinWhere(HashMap<String, Filter> conMap, int isEdgeOrNode) {
        ArrayList<String> whereStrings = new ArrayList<>();
        for (String propName : conMap.keySet()) {
            Filter filter = conMap.get(propName);
            if (filter instanceof Logical) {
                whereStrings.add(joinLogical(propName, (Logical) filter, isEdgeOrNode));
            } else if (filter instanceof Relational) {
                if (isEdgeOrNode == 0) {
                    whereStrings.add(String.format("v.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                } else if (isEdgeOrNode == 1) {
                    whereStrings.add(String.format("e.%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));
                } else {
                    whereStrings.add(String.format("%s %s %s", propName,
                        ((Relational) filter).getSymbol(), Encoding.judgeDataType(((Relational) filter).getValue())));

                }
            } else if (filter instanceof UnaryOperation) {
                if (isEdgeOrNode == 0) {
                    whereStrings.add(String.format("v.%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                } else if (isEdgeOrNode == 1) {
                    whereStrings.add(String.format("e.%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                } else {
                    whereStrings.add(String.format("%s %s", propName,
                        ((UnaryOperation) filter).getSymbol()));
                }
            }
        }
        return String.join(" AND ", whereStrings);
    }

    public static String judgeAndJoinWhere(HashMap<String, Filter> conMap, List<String> filterString, int isEdgeOrNode) {
        StringBuilder result = new StringBuilder();
        if ((conMap != null && !conMap.isEmpty()) || (filterString != null && !filterString.isEmpty())) {
            result.append("WHERE ");
            if (conMap != null && !conMap.isEmpty()) {
                result.append(joinWhere(conMap, isEdgeOrNode));
            }
            if (filterString != null && !filterString.isEmpty()) {
                if (conMap != null && !conMap.isEmpty()) {
                    result.append(" AND ");
                }
                result.append(String.join(" AND ", filterString));
            }
        }
        return result.toString();
    }

    private static String joinLogical(String propName, Logical logical, int isEdgeOrNode) {
        Relational leftRelational = logical.getLeftRelational();
        Relational rightRelational = logical.getRightRelational();
        if (isEdgeOrNode == 0) {
            return String.format("(%s %s %s)",
                String.format("v.%s %s %s", propName,
                    leftRelational.getSymbol(), Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("v.%s %s %s", propName,
                    rightRelational.getSymbol(), Encoding.judgeDataType(rightRelational.getValue())));
        } else if (isEdgeOrNode == 1) {
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

    public static String joinAttributeAlias(List<AttributeColumn> attributeColumns) {
        ArrayList<String> result = new ArrayList<>();
        for (AttributeColumn attributeColumn : attributeColumns) {
            if (attributeColumn.getAlias() != null) {
                result.add(attributeColumn.getPropName() + " AS " + attributeColumn.getAlias());
            } else {
                result.add(attributeColumn.getPropName());
            }
        }
        return String.join(",", result);
    }

    public static String joinAttribute(List<AttributeColumn> attributeColumns) {
        ArrayList<String> result = new ArrayList<>();
        for (AttributeColumn attributeColumn : attributeColumns) {
            result.add(attributeColumn.getPropName());
        }
        return String.join(",", result);
    }

    protected static String joinAggregateFunctionsAlias(List<FunctionColumn> aggregateFunctions) {
        ArrayList<String> result = new ArrayList<>();
        for (FunctionColumn aggregateFunction : aggregateFunctions) {
            if (aggregateFunction.getAlias() == null) {
                result.add(String.format("%s(%s)", aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue()));
            } else {
                result.add(String.format("%s(%s) AS %s", aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue(), aggregateFunction.getAlias()));
            }
        }
        return String.join(",", result);
    }
}
