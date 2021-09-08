/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Relationship;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.operator.PathDirection;
import com.vesoft.nebula.orm.operator.PathType;
import com.vesoft.nebula.orm.query.ngql.FindPath;
import com.vesoft.nebula.orm.query.ngql.FinderPath;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestFindPath extends TestDataBase {
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

    @Test
    public void testFindAllPath() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.count() == 3;
        assert findPath.exist();
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        assert relationships1.size() == 2
            && relationships1.get(0).srcId().asLong() == 1
            && relationships1.get(0).dstId().asLong() == 2
            && relationships1.get(0).ranking() == 1
            && relationships1.get(0).edgeName().equals("team")
            && relationships1.get(1).srcId().asLong() == 2
            && relationships1.get(1).dstId().asLong() == 3
            && relationships1.get(1).ranking() == 0
            && relationships1.get(1).edgeName().equals("team");
        List<Relationship> relationships2 = path.get(2).asPath().getRelationships();
        assert relationships2.size() == 3
            && relationships2.get(0).srcId().asLong() == 1
            && relationships2.get(0).dstId().asLong() == 3
            && relationships2.get(0).ranking() == 0
            && relationships2.get(0).edgeName().equals("work")
            && relationships2.get(1).srcId().asLong() == 3
            && relationships2.get(1).dstId().asLong() == 2
            && relationships2.get(1).ranking() == 1
            && relationships2.get(1).edgeName().equals("work")
            && relationships2.get(2).srcId().asLong() == 2
            && relationships2.get(2).dstId().asLong() == 3
            && relationships2.get(2).ranking() == 0
            && relationships2.get(2).edgeName().equals("team");
    }

    @Test
    public void testFindAllPathAddLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.limit(1).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
    }

    @Test
    public void testFindAllPathAddIllegalLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.limit(-1).all();
        assert findPath.exist();
        assert findPath.count() == 3;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        assert relationships1.size() == 2
            && relationships1.get(0).srcId().asLong() == 1
            && relationships1.get(0).dstId().asLong() == 2
            && relationships1.get(0).ranking() == 1
            && relationships1.get(0).edgeName().equals("team")
            && relationships1.get(1).srcId().asLong() == 2
            && relationships1.get(1).dstId().asLong() == 3
            && relationships1.get(1).ranking() == 0
            && relationships1.get(1).edgeName().equals("team");
        List<Relationship> relationships2 = path.get(2).asPath().getRelationships();
        assert relationships2.size() == 3
            && relationships2.get(0).srcId().asLong() == 1
            && relationships2.get(0).dstId().asLong() == 3
            && relationships2.get(0).ranking() == 0
            && relationships2.get(0).edgeName().equals("work")
            && relationships2.get(1).srcId().asLong() == 3
            && relationships2.get(1).dstId().asLong() == 2
            && relationships2.get(1).ranking() == 1
            && relationships2.get(1).edgeName().equals("work")
            && relationships2.get(2).srcId().asLong() == 2
            && relationships2.get(2).dstId().asLong() == 3
            && relationships2.get(2).ranking() == 0
            && relationships2.get(2).edgeName().equals("team");
    }

    @Test
    public void testFindAllPathAddSteps() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.steps(2).all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        assert relationships1.size() == 2
            && relationships1.get(0).srcId().asLong() == 1
            && relationships1.get(0).dstId().asLong() == 2
            && relationships1.get(0).ranking() == 1
            && relationships1.get(0).edgeName().equals("team")
            && relationships1.get(1).srcId().asLong() == 2
            && relationships1.get(1).dstId().asLong() == 3
            && relationships1.get(1).ranking() == 0
            && relationships1.get(1).edgeName().equals("team");
    }

    @Test
    public void testFindAllPathAddStepsAddLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.steps(2).limit(1).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
    }

    @Test
    public void testFindShortestPath() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.SHORTEST, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
    }

    @Test
    public void testFindNoloopPath() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 3
            && relationships.get(0).ranking() == 0
            && relationships.get(0).edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        assert relationships1.size() == 2
            && relationships1.get(0).srcId().asLong() == 1
            && relationships1.get(0).dstId().asLong() == 2
            && relationships1.get(0).ranking() == 1
            && relationships1.get(0).edgeName().equals("team")
            && relationships1.get(1).srcId().asLong() == 2
            && relationships1.get(1).dstId().asLong() == 3
            && relationships1.get(1).ranking() == 0
            && relationships1.get(1).edgeName().equals("team");
    }

    @Test
    public void testFindNoloopPathAddOrderBy() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.orderBy(true).all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 2
            && relationships.get(0).srcId().asLong() == 1
            && relationships.get(0).dstId().asLong() == 2
            && relationships.get(0).ranking() == 1
            && relationships.get(0).edgeName().equals("team")
            && relationships.get(1).srcId().asLong() == 2
            && relationships.get(1).dstId().asLong() == 3
            && relationships.get(1).ranking() == 0
            && relationships.get(1).edgeName().equals("team");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        assert relationships1.size() == 1
            && relationships1.get(0).srcId().asLong() == 1
            && relationships1.get(0).dstId().asLong() == 3
            && relationships1.get(0).ranking() == 0
            && relationships1.get(0).edgeName().equals("work");
    }

    @Test
    public void testFindNoloopReverselyDirectionPathAddOrderBy() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(2);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.orderBy(true).pathDirection(PathDirection.REVERSELY).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1;
        assert relationships.get(0).srcId().asLong() == 3
            && relationships.get(0).dstId().asLong() == 2
            && relationships.get(0).ranking() == 1
            && relationships.get(0).edgeName().equals("work");
    }

    @Test
    public void testFindPathSrcIdException() {
        ArrayList<Integer> srcIds = new ArrayList<>();
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(true).pathDirection(PathDirection.REVERSELY).all();
        } catch (Exception e) {
            assert e.getMessage().equals("srcIds can not be null");
        }
    }

    @Test
    public void testFindPathDstIdException() {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(true).pathDirection(PathDirection.REVERSELY).all();
        } catch (Exception e) {
            assert e.getMessage().equals("dstIds can not be null");
        }
    }

    @Test
    public void testFindPathPathTypeException() {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(2);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("team");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(null, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(true).pathDirection(PathDirection.REVERSELY).all();
        } catch (Exception e) {
            assert e.getMessage().equals("pathType can not be null");
        }
    }
}
