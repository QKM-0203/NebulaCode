/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.match.RelationshipMatch;
import com.vesoft.nebula.orm.match.VertexMatch;
import com.vesoft.nebula.orm.operator.*;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.ngql.*;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * used to splice parameters into query statements.
 *
 * @author Qi Kai Meng
 */
public class QueryBase<E> {
    protected Map<Column, Sort> orderByMatch;
    private final Graph graph;

    public Graph getGraph() {
        return graph;
    }

    protected QueryBase(Graph graph) {
        this.graph = graph;
    }

    private String joinWhere(Map<String, Filter> conMap, int isEdge) {
        ArrayList<String> whereStrings = new ArrayList<>();
        for (String propName : conMap.keySet()) {
            Filter filter = conMap.get(propName);
            if (filter instanceof Logical) {
                whereStrings.add(joinLogical(propName, (Logical) filter, isEdge));
            } else if (filter instanceof Relational) {
                if (isEdge == 0) {
                    whereStrings.add(String.format("v.%s %s %s", propName,
                        ((Relational) filter).getSymbol(),
                        Encoding.judgeDataType(((Relational) filter).getValue())));
                } else if (isEdge == 1) {
                    whereStrings.add(String.format("e.%s %s %s", propName,
                        ((Relational) filter).getSymbol(),
                        Encoding.judgeDataType(((Relational) filter).getValue())));
                } else {
                    whereStrings.add(String.format("%s %s %s", propName,
                        ((Relational) filter).getSymbol(),
                        Encoding.judgeDataType(((Relational) filter).getValue())));

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
        return String.join(" " + KeyWord.AND + " ", whereStrings);
    }

    public String judgeAndJoinWhere(Map<String, Filter> conMap,
                                    List<String> filterString, int isEdge) {
        StringBuilder result = new StringBuilder();
        if ((conMap != null && !conMap.isEmpty())
            || (filterString != null && !filterString.isEmpty())) {
            result.append(" ").append(KeyWord.WHERE).append(" ");
            if (conMap != null && !conMap.isEmpty()) {
                result.append(joinWhere(conMap, isEdge));
            }
            if (filterString != null && !filterString.isEmpty()) {
                if (conMap != null && !conMap.isEmpty()) {
                    result.append(" " + KeyWord.AND + " ");
                }
                result.append(String.join(" " + KeyWord.AND + " ", filterString));
            }
        }
        return result.toString();
    }

    private String joinLogical(String propName, Logical logical, int isEdge) {
        Relational leftRelational = logical.getLeftRelational();
        Relational rightRelational = logical.getRightRelational();
        if (isEdge == 0) {
            return String.format("(%s %s %s)",
                String.format("v.%s %s %s", propName,
                    leftRelational.getSymbol(),
                    Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("v.%s %s %s", propName,
                    rightRelational.getSymbol(),
                    Encoding.judgeDataType(rightRelational.getValue())));
        } else if (isEdge == 1) {
            return String.format("(%s %s %s)",
                String.format("e.%s %s %s", propName,
                    leftRelational.getSymbol(),
                    Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("e.%s %s %s", propName,
                    rightRelational.getSymbol(),
                    Encoding.judgeDataType(rightRelational.getValue())));
        } else {
            return String.format("(%s %s %s)",
                String.format("%s %s %s", propName,
                    leftRelational.getSymbol(),
                    Encoding.judgeDataType(leftRelational.getValue())),
                logical.getSymbol(),
                String.format("%s %s %s", propName,
                    rightRelational.getSymbol(),
                    Encoding.judgeDataType(rightRelational.getValue())));
        }
    }

    public String joinAttributeAlias(List<Column> columns) {
        ArrayList<String> result = new ArrayList<>();
        for (Column column : columns) {
            if (column.getAlias() != null) {
                result.add(column.getPropName() + " " + KeyWord.AS + " " + column.getAlias());
            } else {
                result.add(column.getPropName());
            }
        }
        return String.join(",", result);
    }

    public String joinAttribute(List<Column> columns) {
        ArrayList<String> result = new ArrayList<>();
        for (Column column : columns) {
            result.add(column.getPropName());
        }
        return String.join(",", result);
    }

    protected String joinAggregateFunctionsAlias(List<Column> aggregateFunctions) {
        ArrayList<String> result = new ArrayList<>();
        for (Column aggregateFunction : aggregateFunctions) {
            if (aggregateFunction.getAlias() == null) {
                result.add(String.format("%s(%s)",
                    aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue()));
            } else {
                result.add(String.format("%s(%s) " + KeyWord.AS + " %s",
                    aggregateFunction.getAggregateFunction().toString(),
                    aggregateFunction.getAggregateFunction().getValue(),
                    aggregateFunction.getAlias()));
            }
        }
        return String.join(",", result);
    }

    public String joinOrderBy(Map<String, Sort> orderBy) {
        ArrayList<String> orderByStrings = new ArrayList<>();
        for (String propName : orderBy.keySet()) {
            if (orderBy.get(propName) != null) {
                orderByStrings.add(propName + " " + orderBy.get(propName));
            } else {
                orderByStrings.add(propName);
            }
        }
        return String.join(",", orderByStrings);
    }

    /**
     * this function is used for order by.
     *
     * @param orderByMatch if you query match pass in this,other pass in null is ok.
     *                     <p>if order by and group by are used together,the field of orderBy
     *                     passed must be included in {@link RelationshipMatch#groupBy(List, List)}
     *                     groupBy and aggregateFunctions, or {@link VertexMatch#groupBy(List, List)}
     *                     groupBy and aggregateFunctions,this function will sort directly using
     *                     the passed alias,you can pass in <{@link Column}(null,alias),{@link Sort}>.</p>
     *                     <p>if you only use order by,this function puts the orderBy field you passed in
     *                     after the return field as output,and then sorts the aliases,you must pass in
     *                     <{@link Column}("propName",alias),{@link Sort}>.</p>
     *                     <p>if you do not use order by, return e or return v.</p>
     *                     <p>order by uses an alias to sort, so you must pass in an alias,
     *                     if the sort you passed is null,default is ASC.</p>
     * @param orderByNGql  if you query NGql pass in this,other pass in null is ok.
     * @param isPathSort   sort result paths by $-.PATH,isSort is true or false,default value is false.
     * @return MatchObject or NGqlObject
     */
    public E orderBy(Map<Column, Sort> orderByMatch, Map<String, Sort> orderByNGql,
                     Boolean isPathSort) {
        if (orderByNGql != null && !orderByNGql.isEmpty()) {
            if (this instanceof Go) {
                Go go = (Go) this;
                go.getCondition().append(" ").append(KeyWord.PIPE).append(" ")
                    .append(KeyWord.ORDER_BY).append(" ").append(joinOrderBy(orderByNGql));
            } else if (this instanceof LookUp) {
                LookUp lookUp = (LookUp) this;
                lookUp.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                    .append(KeyWord.ORDER_BY).append(" ").append(joinOrderBy(orderByNGql));
            } else if (this instanceof FetchEdge) {
                FetchEdge fetchEdge = (FetchEdge) this;
                fetchEdge.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                    .append(KeyWord.ORDER_BY).append(" ").append(joinOrderBy(orderByNGql));
            } else if (this instanceof FetchVertex) {
                FetchVertex fetchVertex = (FetchVertex) this;
                fetchVertex.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                    .append(KeyWord.ORDER_BY).append(" ").append(joinOrderBy(orderByNGql));
            }
        }
        if (this instanceof FindPath) {
            if (isPathSort) {
                FindPath findPath = (FindPath) this;
                findPath.getClause().append(" ").append(KeyWord.PIPE).append(" ")
                    .append(KeyWord.ORDER_BY).append(" ").append(KeyWord.$_PATH);
            }
        }
        if (orderByMatch != null && !orderByMatch.isEmpty()) {
            this.orderByMatch = orderByMatch;
        }
        return (E) this;
    }

    /**
     * @return from result get the first.
     */
    public ResultSet.Record first() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowValues(0);
        }
        return null;
    }

    /**
     * get all result,if you get all by fetchRelationship if you can
     * call {@link FetchEdge#fetchAll()}.
     *
     * @return object this object is List<ResultSet> or ResultSet.
     */
    public ResultSet all() {
        String matchRelationship = null;
        if (this instanceof Go) {
            Go go = (Go) this;
            matchRelationship = go.connectParameters();
        } else if (this instanceof LookUp) {
            LookUp lookUp = (LookUp) this;
            matchRelationship = lookUp.connectParameters();
        } else if (this instanceof FetchVertex) {
            FetchVertex fetchVertex = (FetchVertex) this;
            matchRelationship = fetchVertex.connectParameters();
        } else if (this instanceof VertexMatch) {
            VertexMatch vertexMatch = (VertexMatch) this;
            matchRelationship = vertexMatch.connectParameters();
        } else if (this instanceof RelationshipMatch) {
            RelationshipMatch relationshipMatch = (RelationshipMatch) this;
            matchRelationship = relationshipMatch.connectParameters();
        } else if (this instanceof FindPath) {
            FindPath findPath = (FindPath) this;
            matchRelationship = findPath.connectParameters();
        } else if (this instanceof GetSubgraph) {
            GetSubgraph getSubgraph = (GetSubgraph) this;
            matchRelationship = getSubgraph.connectParameters();
        }
        ResultSet resultSet = graph.run(matchRelationship);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet;
    }


    /**
     * count of results.
     *
     * @return count
     */
    public long count() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowsSize();
        }
        return 0;
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}
