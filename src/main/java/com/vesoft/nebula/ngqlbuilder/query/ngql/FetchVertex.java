/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.query.ngql;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import com.vesoft.nebula.ngqlbuilder.exception.InitException;
import com.vesoft.nebula.ngqlbuilder.query.cypher.Encoding;
import com.vesoft.nebula.ngqlbuilder.query.util.KeyWord;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * get the attribute value according to the tag and IDs.
 *
 * <p>sentence of query multipoint id multi label is similar to
 * 'FETCH PROP ON player, t1 "player100", "player103"'.</p>
 * <p>sentence of query one point ID one label is similar to
 * 'FETCH PROP ON player "player100"'.</p>
 *
 * @author Qi Kai Meng
 */
public class FetchVertex extends NGqlQuery<FetchVertex> {
    private List<String> tagNames;
    private List<Object> vidList;
    private StringBuffer clause = new StringBuffer();

    protected FetchVertex(Graph graph) {
        super(graph);
    }

    public StringBuffer getClause() {
        return clause;
    }


    public FetchVertex on(String... tagNames) {
        this.tagNames = Arrays.asList(tagNames);
        return this;
    }

    protected FetchVertex init(List<?> vidList) {
        this.vidList = (List<Object>) vidList;
        return this;
    }

    protected FetchVertex init(Object id) {
        this.vidList = new ArrayList<>();
        this.vidList.add(id);
        return this;
    }

    public String connectParameters() {
        if (vidList == null || vidList.isEmpty()) {
            throw new InitException("vidList can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.FETCH_PROP_ON).append(" ");
        if (tagNames == null || tagNames.isEmpty()) {
            result.append(KeyWord.ALL).append(" ");
        } else {
            result.append(String.join(",", tagNames)).append(" ");
        }
        result.append(String.join(",", Encoding.encodeIdList(vidList)));
        if (yields != null && !yields.isEmpty()) {
            result.append(" ").append(KeyWord.YIELD).append(" ").append(String.join(",", yields));
        }
        if (clause != null) {
            result.append(clause);
        }
        return result.toString().trim();
    }
}
