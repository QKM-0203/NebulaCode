/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.exception.ExecuteException;
import java.util.ArrayList;
import java.util.List;

/**
 * get the attribute value according to the {@link Relationship} object.
 * <p>if you want to query the properties of an edge, pass in a relationship object
 * and finally call the {@link #one()} method.</p>
 * <p>if you want to query the properties of some edges,
 * pass in the relationshipList and finally call the {@link #all()} method,
 * if you can get first result you can call {@link #one()}.</p>
 * <p>sentence of query is similar to 'FETCH PROP ON serve "player100" -> "team204[@0]"'
 * if you query multiple edges are separated by commas.</p>
 *
 * @author Qi Kai Meng
 */
public class FetchEdge extends NGqlQuery<FetchEdge> {
    private List<Relationship> relationships;
    private final Graph graph;

    protected FetchEdge(Graph graph) {
        this.graph = graph;
    }

    protected FetchEdge init(List<Relationship> relationships) {
        this.relationships = relationships;
        return this;
    }

    protected FetchEdge init(Relationship relationship) {
        this.relationships = new ArrayList<>();
        this.relationships.add(relationship);
        return this;
    }


    /**
     * classify the relationships with the same edgeName together,
     * and then assemble them into fetchStrings to return.
     * <p>attention if you pass in yield, the fetchStrings of multiple edges
     * will be the same yield.</p>
     *
     * @return fetchStrings
     */
    public List<String> connectParameters() {
        return joinFetch(relationships);
    }

    /**
     * query some relationships.
     *
     * @return all qualified
     */
    public List<ResultSet> all() {
        List<String> query = connectParameters();
        ArrayList<ResultSet> resultSets = new ArrayList<>();
        for (String fetch : query) {
            ResultSet resultSet = graph.run(fetch.trim());
            if (!resultSet.isSucceeded()) {
                throw new ExecuteException(resultSet.getErrorMessage()
                    + "the successful edge is " + resultSets);
            }
            resultSets.add(resultSet);
        }
        return resultSets;
    }

    /**
     * query one relationship,can be null.
     *
     * @return all qualified
     */
    public ResultSet one() {
        List<ResultSet> all = all();
        if (!all.isEmpty()) {
            return all.get(0);
        }
        return null;
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }

    /**
     * gets the number of edges for which the corresponding attribute is found.
     *
     * @return count
     */
    public long count() {
        List<ResultSet> all = all();
        int count = 0;
        for (ResultSet resultSet : all) {
            count += resultSet.rowsSize();
        }
        return count;
    }

}
