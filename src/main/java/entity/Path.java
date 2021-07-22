/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import Operator.EdgeDirection;
import Operator.PathType;

import java.util.List;

public class Path {

    private  PathType pathType;

    private  List<Object> srcVertexIdList;

    private  List<Object> dstVertexIdList;

    //edgeName
    private List<String> edgeTypeList;

    private EdgeDirection edgeDirectionDirection;

    private int pathStep = 5;

    private int rowsNumber;

    public Path(PathType pathType, List<Object> srcVertexIdList, List<Object> dstVertexIdList,
                List<String> edgeTypeList) {
        this.pathType = pathType;
        this.srcVertexIdList = srcVertexIdList;
        this.dstVertexIdList = dstVertexIdList;
        this.edgeTypeList = edgeTypeList;
    }

    public Path(PathType pathType, List<Object> srcVertexIdList, List<Object> dstVertexIdList) {
        this.pathType = pathType;
        this.srcVertexIdList = srcVertexIdList;
        this.dstVertexIdList = dstVertexIdList;
    }

    public Path(PathType pathType, List<Object> srcVertexIdList, List<Object> dstVertexIdList,
                List<String> edgeTypeList, EdgeDirection edgeDirectionDirection) {
        this.pathType = pathType;
        this.srcVertexIdList = srcVertexIdList;
        this.dstVertexIdList = dstVertexIdList;
        this.edgeTypeList = edgeTypeList;
        this.edgeDirectionDirection = edgeDirectionDirection;
    }

    public Path(PathType pathType, List<Object> srcVertexIdList, List<Object> dstVertexIdList,
                List<String> edgeTypeList, EdgeDirection edgeDirectionDirection, int pathStep, int rowsNumber) {
        this.pathType = pathType;
        this.srcVertexIdList = srcVertexIdList;
        this.dstVertexIdList = dstVertexIdList;
        this.edgeTypeList = edgeTypeList;
        this.edgeDirectionDirection = edgeDirectionDirection;
        this.pathStep = pathStep;
        this.rowsNumber = rowsNumber;
    }

    public void setEdgeTypeList(List<String> edgeTypeList) {
        this.edgeTypeList = edgeTypeList;
    }

    public void setEdgeDirection(EdgeDirection edgeDirectionDirection) {
        this.edgeDirectionDirection = edgeDirectionDirection;
    }

    public void setPathStep(int pathStep) {
        this.pathStep = pathStep;
    }

    public void setRowsNumber(int rowsNumber) {
        this.rowsNumber = rowsNumber;
    }

    public void setPathType(PathType pathType) {
        this.pathType = pathType;
    }

    public void setSrcVertexIdList(List<Object> srcVertexIdList) {
        this.srcVertexIdList = srcVertexIdList;
    }

    public void setDstVertexIdList(List<Object> dstVertexIdList) {
        this.dstVertexIdList = dstVertexIdList;
    }
}
