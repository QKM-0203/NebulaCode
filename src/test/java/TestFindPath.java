/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.Relationship;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.ngqlbuilder.operator.PathDirection;
import com.vesoft.nebula.ngqlbuilder.operator.PathType;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FindPath;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FinderPath;
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
        graph.createTag(person);
        graph.createTag(hobby);
        graph.createEdge(subject);
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
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.count() == 3;
        assert findPath.exist();
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        Relationship relationship1 = relationships1.get(0);
        assert relationships1.size() == 2
            && relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 2
            && relationship1.ranking() == 1
            && relationship1.edgeName().equals("subject");
        Relationship relationship2 = relationships1.get(1);
        assert relationship2.srcId().asLong() == 2
            && relationship2.dstId().asLong() == 3
            && relationship2.ranking() == 0
            && relationship2.edgeName().equals("subject");
        List<Relationship> relationships2 = path.get(2).asPath().getRelationships();
        Relationship relationship3 = relationships2.get(0);
        assert relationships2.size() == 3
            && relationship3.srcId().asLong() == 1
            && relationship3.dstId().asLong() == 3
            && relationship3.ranking() == 0
            && relationship3.edgeName().equals("work");
        Relationship relationship4 = relationships2.get(1);
        assert relationship4.srcId().asLong() == 3
            && relationship4.dstId().asLong() == 2
            && relationship4.ranking() == 1
            && relationship4.edgeName().equals("work");
        Relationship relationship5 = relationships2.get(2);
        assert relationship5.srcId().asLong() == 2
            && relationship5.dstId().asLong() == 3
            && relationship5.ranking() == 0
            && relationship5.edgeName().equals("subject");
    }

    @Test
    public void testFindAllPathAddLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.limit(0, 1).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
    }

    @Test
    public void testFindAllPathAddIllegalLimit() {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.limit(-1, -1).all();
        } catch (Exception e) {
            assert e.getMessage().equals("SyntaxError: syntax error near `-1,-1'");
        }
    }

    @Test
    public void testFindAllPathAddSteps() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.steps(2).all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        Relationship relationship1 = relationships1.get(0);
        assert relationships1.size() == 2
            && relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 2
            && relationship1.ranking() == 1
            && relationship1.edgeName().equals("subject");
        Relationship relationship2 = relationships1.get(1);
        assert relationship2.srcId().asLong() == 2
            && relationship2.dstId().asLong() == 3
            && relationship2.ranking() == 0
            && relationship2.edgeName().equals("subject");
    }

    @Test
    public void testFindAllPathAddStepsAddLimit() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.ALL, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.steps(2).limit(0, 1).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
    }

    @Test
    public void testFindShortestPath() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.SHORTEST, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
    }

    @Test
    public void testFindNoloopPath() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 1
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 3
            && relationship.ranking() == 0
            && relationship.edgeName().equals("work");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        Relationship relationship1 = relationships1.get(0);
        assert relationships1.size() == 2
            && relationship1.srcId().asLong() == 1
            && relationship1.dstId().asLong() == 2
            && relationship1.ranking() == 1
            && relationship1.edgeName().equals("subject");
        Relationship relationship2 = relationships1.get(1);
        assert relationship2.srcId().asLong() == 2
            && relationship2.dstId().asLong() == 3
            && relationship2.ranking() == 0
            && relationship2.edgeName().equals("subject");
    }

    @Test
    public void testFindNoloopPathAddOrderBy() throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(1);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.orderBy(null,null,true).all();
        assert findPath.exist();
        assert findPath.count() == 2;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        Relationship relationship = relationships.get(0);
        assert relationships.size() == 2
            && relationship.srcId().asLong() == 1
            && relationship.dstId().asLong() == 2
            && relationship.ranking() == 1
            && relationship.edgeName().equals("subject");
        Relationship relationship1 = relationships.get(1);
        assert relationship1.srcId().asLong() == 2
            && relationship1.dstId().asLong() == 3
            && relationship1.ranking() == 0
            && relationship1.edgeName().equals("subject");
        List<Relationship> relationships1 = path.get(1).asPath().getRelationships();
        Relationship relationship2 = relationships1.get(0);
        assert relationships1.size() == 1
            && relationship2.srcId().asLong() == 1
            && relationship2.dstId().asLong() == 3
            && relationship2.ranking() == 0
            && relationship2.edgeName().equals("work");
    }

    @Test
    public void testFindNoloopReverselyDirectionPathAddOrderBy()
        throws UnsupportedEncodingException {
        ArrayList<Integer> srcIds = new ArrayList<>();
        srcIds.add(2);
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        ResultSet all = findPath.orderBy(null,null,true).pathDirection(PathDirection.REVERSELY).all();
        assert findPath.exist();
        assert findPath.count() == 1;
        List<ValueWrapper> path = all.colValues("path");
        List<Relationship> relationships = path.get(0).asPath().getRelationships();
        assert relationships.size() == 1;
        Relationship relationship = relationships.get(0);
        assert relationship.srcId().asLong() == 3
            && relationship.dstId().asLong() == 2
            && relationship.ranking() == 1
            && relationship.edgeName().equals("work");
    }

    @Test
    public void testFindPathSrcIdException() {
        ArrayList<Integer> dstIds = new ArrayList<>();
        dstIds.add(3);
        ArrayList<String> edgeNames = new ArrayList<>();
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        ArrayList<Integer> srcIds = new ArrayList<>();
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(null,null,true).pathDirection(PathDirection.REVERSELY).all();
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
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(PathType.NOLOOP, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(null,null,true).pathDirection(PathDirection.REVERSELY).all();
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
        edgeNames.add("subject");
        edgeNames.add("work");
        FinderPath finderPath = new FinderPath(graph);
        FindPath findPath = finderPath.find(null, srcIds, dstIds, edgeNames);
        try {
            ResultSet all = findPath.orderBy(null,null,true).pathDirection(PathDirection.REVERSELY).all();
        } catch (Exception e) {
            assert e.getMessage().equals("pathType can not be null");
        }
    }
}
