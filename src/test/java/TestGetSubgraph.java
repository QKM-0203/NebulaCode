/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.query.ngql.GetSubgraph;
import com.vesoft.nebula.orm.query.ngql.GetterSubgraph;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestGetSubgraph extends TestDataBase {
    {
        {
            graph.createTag(qkm1);
            graph.createTag(qkm2);
            graph.createEdge(team);
            graph.createEdge(work);
            graph.create(vertexOne);
            graph.create(vertexTwo);
            graph.create(vertexThird);
            graph.create(vertexFour);
            graph.create(relationship12);
            graph.create(relationship32);
            graph.create(relationship24);
            graph.create(relationship34);
            graph.create(relationship23);
            graph.create(relationship13);
        }

    }

    @Test
    public void testGetSubgraph() throws UnsupportedEncodingException {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        GetterSubgraph getterSubgraph = new GetterSubgraph(graph);
        GetSubgraph getSubgraph = getterSubgraph.get(ids);
        ResultSet all = getSubgraph.edges(EdgeDirection.OUT, "team", "work").all();
        assert getSubgraph.exist();
        List<ValueWrapper> vertices = all.colValues("_vertices");
        List<ValueWrapper> edges = all.colValues("_edges");
        assert vertices.get(0).asList().get(0).asNode().getId().asLong() == 1;
        assert vertices.get(1).asList().get(0).asNode().getId().asLong() == 3
            && vertices.get(1).asList().get(1).asNode().getId().asLong() == 2;
        assert edges.get(0).asList().get(0).asRelationship().srcId().asLong() == 1
            && edges.get(0).asList().get(0).asRelationship().dstId().asLong() == 3
            && edges.get(0).asList().get(0).asRelationship().ranking() == 0
            && edges.get(0).asList().get(0).asRelationship().edgeName().equals("work")
            && edges.get(0).asList().get(1).asRelationship().srcId().asLong() == 1
            && edges.get(0).asList().get(1).asRelationship().dstId().asLong() == 2
            && edges.get(0).asList().get(1).asRelationship().ranking() == 1
            && edges.get(0).asList().get(1).asRelationship().edgeName().equals("team");
        assert edges.get(1).asList().get(0).asRelationship().srcId().asLong() == 3
            && edges.get(1).asList().get(0).asRelationship().dstId().asLong() == 2
            && edges.get(1).asList().get(0).asRelationship().ranking() == 1
            && edges.get(1).asList().get(0).asRelationship().edgeName().equals("work")
            && edges.get(1).asList().get(1).asRelationship().srcId().asLong() == 2
            && edges.get(1).asList().get(1).asRelationship().dstId().asLong() == 3
            && edges.get(1).asList().get(1).asRelationship().ranking() == 0
            && edges.get(1).asList().get(1).asRelationship().edgeName().equals("team");
    }

    @Test
    public void testGetSubgraphAddSteps() throws UnsupportedEncodingException {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        GetterSubgraph getterSubgraph = new GetterSubgraph(graph);
        GetSubgraph getSubgraph = getterSubgraph.get(ids);
        ResultSet all = getSubgraph.edges(EdgeDirection.OUT, "team", "work").steps(2).all();
        assert getSubgraph.exist();
        List<ValueWrapper> vertices = all.colValues("_vertices");
        assert vertices.get(0).asList().get(0).asNode().getId().asLong() == 1;
        assert vertices.get(1).asList().get(0).asNode().getId().asLong() == 3
            && vertices.get(1).asList().get(1).asNode().getId().asLong() == 2;
        assert vertices.get(2).asList().get(0).asNode().getId().asLong() == 4;
        List<ValueWrapper> edges = all.colValues("_edges");
        assert edges.get(0).asList().get(0).asRelationship().srcId().asLong() == 1
            && edges.get(0).asList().get(0).asRelationship().dstId().asLong() == 3
            && edges.get(0).asList().get(0).asRelationship().ranking() == 0
            && edges.get(0).asList().get(0).asRelationship().edgeName().equals("work")
            && edges.get(0).asList().get(1).asRelationship().srcId().asLong() == 1
            && edges.get(0).asList().get(1).asRelationship().dstId().asLong() == 2
            && edges.get(0).asList().get(1).asRelationship().ranking() == 1
            && edges.get(0).asList().get(1).asRelationship().edgeName().equals("team");
        assert edges.get(1).asList().get(0).asRelationship().srcId().asLong() == 3
            && edges.get(1).asList().get(0).asRelationship().dstId().asLong() == 2
            && edges.get(1).asList().get(0).asRelationship().ranking() == 1
            && edges.get(1).asList().get(0).asRelationship().edgeName().equals("work")
            && edges.get(1).asList().get(1).asRelationship().srcId().asLong() == 3
            && edges.get(1).asList().get(1).asRelationship().dstId().asLong() == 4
            && edges.get(1).asList().get(1).asRelationship().ranking() == 1
            && edges.get(1).asList().get(1).asRelationship().edgeName().equals("work")
            && edges.get(1).asList().get(2).asRelationship().srcId().asLong() == 2
            && edges.get(1).asList().get(2).asRelationship().dstId().asLong() == 3
            && edges.get(1).asList().get(2).asRelationship().ranking() == 0
            && edges.get(1).asList().get(2).asRelationship().edgeName().equals("team")
            && edges.get(1).asList().get(3).asRelationship().srcId().asLong() == 2
            && edges.get(1).asList().get(3).asRelationship().dstId().asLong() == 4
            && edges.get(1).asList().get(3).asRelationship().ranking() == 1
            && edges.get(1).asList().get(3).asRelationship().edgeName().equals("team");
        assert edges.get(2).asList().isEmpty();
    }

    @Test
    public void testSrcIdException() {
        ArrayList<Integer> ids = new ArrayList<>();
        GetterSubgraph getterSubgraph = new GetterSubgraph(graph);
        GetSubgraph getSubgraph = getterSubgraph.get(ids);
        try {
            ResultSet all = getSubgraph.edges(EdgeDirection.OUT, "team", "work").steps(2).all();
        } catch (Exception e) {
            assert e.getMessage().equals("srcIds can not be null");
        }

    }


}
