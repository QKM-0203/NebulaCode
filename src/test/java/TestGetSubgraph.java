/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Node;
import com.vesoft.nebula.client.graph.data.Relationship;
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
        Node node = vertices.get(0).asList().get(0).asNode();
        Node node1 = vertices.get(0).asList().get(0).asNode();
        Node node2 = vertices.get(0).asList().get(0).asNode();
        Relationship relationship = edges.get(0).asList().get(0).asRelationship();
        Relationship relationship1 = edges.get(0).asList().get(1).asRelationship();
        Relationship relationship2 = edges.get(1).asList().get(0).asRelationship();
        Relationship relationship3 = edges.get(1).asList().get(1).asRelationship();
        assert node.getId().asLong() == 1
            && node1.getId().asLong() == 3
            && node2.getId().asLong() == 2;
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work")
            && relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 2
            && relationship1.ranking() == 1
            && relationship1.edgeName().equals("team");
        assert relationship2.srcId().asLong() == 3
            && relationship2.dstId().asLong() == 2
            && relationship2.ranking() == 1
            && relationship2.edgeName().equals("work")
            && relationship3.srcId().asLong() == 2
            && relationship3.dstId().asLong() == 3
            && relationship3.ranking() == 0
            && relationship3.edgeName().equals("team");
    }

    @Test
    public void testGetSubgraphAddLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        GetterSubgraph getterSubgraph = new GetterSubgraph(graph);
        GetSubgraph getSubgraph = getterSubgraph.get(ids);
        ResultSet all = getSubgraph.edges(EdgeDirection.OUT, "team", "work")
            .limit(1, 1).all();
        assert getSubgraph.exist();
        List<ValueWrapper> vertices = all.colValues("_vertices");
        List<ValueWrapper> edges = all.colValues("_edges");
        Node node = vertices.get(0).asList().get(0).asNode();
        Node node1 = vertices.get(0).asList().get(1).asNode();
        Relationship relationship = edges.get(0).asList().get(0).asRelationship();
        Relationship relationship1 = edges.get(0).asList().get(1).asRelationship();
        assert node.getId().asLong() == 3
            && node1.getId().asLong() == 2;
        assert relationship.srcId().asLong() == 3
            && relationship.dstId().asLong() == 2
            && relationship.ranking() == 1
            && relationship.edgeName().equals("work")
            && relationship1.srcId().asLong() == 2
            && relationship1.dstId().asLong() == 3
            && relationship1.ranking() == 0
            && relationship1.edgeName().equals("team");
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
        Node node = vertices.get(0).asList().get(0).asNode();
        Node node1 = vertices.get(1).asList().get(0).asNode();
        Node node2 = vertices.get(1).asList().get(1).asNode();
        Node node3 = vertices.get(2).asList().get(0).asNode();
        assert node.getId().asLong() == 1
            && node1.getId().asLong() == 3
            && node2.getId().asLong() == 2
            && node3.getId().asLong() == 4;
        List<ValueWrapper> edges = all.colValues("_edges");
        Relationship relationship = edges.get(0).asList().get(0).asRelationship();
        Relationship relationship1 = edges.get(0).asList().get(1).asRelationship();
        Relationship relationship2 = edges.get(1).asList().get(0).asRelationship();
        Relationship relationship3 = edges.get(1).asList().get(1).asRelationship();
        Relationship relationship4 = edges.get(1).asList().get(2).asRelationship();
        Relationship relationship5 = edges.get(1).asList().get(3).asRelationship();
        assert relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work")
            && relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 2
            && relationship1.ranking() == 1
            && relationship1.edgeName().equals("team");
        assert relationship2.srcId().asLong() == 3
            && relationship2.dstId().asLong() == 2
            && relationship2.ranking() == 1
            && relationship2.edgeName().equals("work")
            && relationship3.srcId().asLong() == 3
            && relationship3.dstId().asLong() == 4
            && relationship3.ranking() == 1
            && relationship3.edgeName().equals("work")
            && relationship4.srcId().asLong() == 2
            && relationship4.dstId().asLong() == 3
            && relationship4.ranking() == 0
            && relationship4.edgeName().equals("team")
            && relationship5.srcId().asLong() == 2
            && relationship5.dstId().asLong() == 4
            && relationship5.ranking() == 1
            && relationship5.edgeName().equals("team");
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
