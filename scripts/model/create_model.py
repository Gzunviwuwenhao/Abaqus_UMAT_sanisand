# -* - coding:UTF-8 -*-
import os  # 标准库
import sys  # 系统路径

import regionToolset  # type: ignore
from abaqus import *  # type: ignore
from abaqusConstants import *  # type: ignore
from caeModules import *  # type: ignore
from driverUtils import *  # type: ignore


# ======================================================================================================================
def drained_rectangular_true(
    pressure: float, model_path: str, test_path: str, inp_name: str, cae_name: str
):
    """
    基于 abaqus python 接口，通过 python 文件批量生成不同工况的 abaqus 的 .cae 和 .inp 文件，并存放至相应的文件夹当中。
    注意:使用的abaqus 2024的版本,abaqus 内置python版本为python3.10
    # session.journalOptions.setValues(replayGeometry=COORDINATE,recoverGeometry= COORDINATE)
    :param test_path  :
    :param cae_name   :
    :param model_path :
    :param inp_name:
    :param pressure : 围压
    """
    # 设置编码格式（输出中文）
    sys.stdout.encoding = "utf-8"  # pyright: ignore[reportAttributeAccessIssue]
    # open abaqus2025 without gui
    executeOnCaeStartup()
    #
    # create model
    model_name: str = "Model-1"
    model = mdb.models[model_name]
    # -----------------------------------------------create sketch------------------------------------------------------
    #
    # (1) - soil
    sketch_name: str = "sketch_soil"
    sketch1: Any = model.ConstrainedSketch(name=sketch_name, sheetSize=1.0)  # type: ignore[reportUnknownArgumentType]
    sketch1.rectangle(point1=(0.0, 0.0), point2=(1.0, 1.0))
    # ------------------------------------------create part_soil--------------------------------------------------------
    #
    # (1) - soil
    part_soil = model.Part(name="soil", dimensionality=THREE_D, type=DEFORMABLE_BODY)
    # Generating entity
    part_soil.BaseSolidExtrude(sketch=sketch1, depth=1.0)
    #
    # ----------------------------------------------Create sets---------------------------------------------------------
    #
    # (1) - soil
    # first : cells sets
    part_soil.Set(
        name="set-all",
        cells=part_soil.cells.findAt(coordinates=((1.0, 0.666667, 0.666667),)),
    )
    part_soil.Set(
        name="set-initial-stress",
        cells=part_soil.cells.findAt(coordinates=((1.0, 0.666667, 0.666667),)),
    )
    # Create boundary conditions set
    part_soil.Set(
        name="set-X-bottom",
        faces=part_soil.faces.findAt(coordinates=((0.0, 0.333333, 0.666667),)),
    )
    part_soil.Set(
        name="set-Y-bottom",
        faces=part_soil.faces.findAt(coordinates=((0.666667, 0.0, 0.666667),)),
    )
    part_soil.Set(
        name="set-Z-bottom",
        faces=part_soil.faces.findAt(coordinates=((0.666667, 0.333333, 0.0),)),
    )
    # Create Pressure sets
    # noticing!!    set-X-top is also the loading set
    part_soil.Set(
        name="set-X-top",
        faces=part_soil.faces.findAt(coordinates=((1.0, 0.666667, 0.666667),)),
    )
    part_soil.Surface(  # type: ignore[reportUnknownArgumentType]
        name="set-Y-top",
        side1Faces=part_soil.faces.findAt(coordinates=((0.333333, 1.0, 0.666667),)),
    )
    part_soil.Set(
        name="set-Z-top", faces=part_soil.faces.findAt(coordinates=((0.5, 0.5, 1.0),))
    )
    # surface--P
    part_soil.Surface(  # type: ignore[reportUnknownArgumentType]
        name="Surface-P",
        side1Faces=part_soil.faces.findAt(
            ((0.333333, 1.0, 0.666667),),
            ((1.0, 0.666667, 0.666667),),
            ((0.333333, 0.333333, 1.0),),
        ),
    )
    # -------------------------------------------Assembly---------------------------------------------------------------
    #
    model.rootAssembly.DatumCsysByDefault(CARTESIAN)
    myassembly = model.rootAssembly
    myassembly.Instance(name="instance_soil", part=part_soil, dependent=ON)
    # ---------------------------------------------Mesh-----------------------------------------------------------------
    #
    # (1) - soil
    elemtype1_soil = mesh.ElemType(elemCode=C3D8, elemLibrary=STANDARD)
    elemtype2_soil = mesh.ElemType(elemCode=C3D6, elemLibrary=STANDARD)
    elemtype3_soil = mesh.ElemType(elemCode=C3D4, elemLibrary=STANDARD)
    #
    part_soil.setElementType(
        regions=(part_soil.cells,),  # type: ignore[reportUnknownArgumentType]
        elemTypes=(elemtype1_soil, elemtype2_soil, elemtype3_soil),
    )
    part_soil.seedPart(size=1.0, deviationFactor=0.1, minSizeFactor=0.1)
    part_soil.generateMesh()
    # ------------------------------------------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------------------------------------------
    # Material
    material1 = model.Material(name="fabric_evolution_model")
    material1.Depvar(n=20)
    material1.Density(table=((2.0,),))  # type: ignore[reportUnknownArgumentType]
    material1.UserMaterial(  # type: ignore[reportUnknownArgumentType]
        mechanicalConstants=(
            0.15,  # G0     = PROPS(1)   ! elastic bulk modulus
        ),
        unsymm=ON,  # Asymmetric solution
    )
    # section
    model.HomogeneousSolidSection(
        name="section", material="fabric_evolution_model", thickness=None
    )
    part_soil.SectionAssignment(
        region=part_soil.sets["set-all"],
        sectionName="section",
        offset=0.0,
        offsetType=MIDDLE_SURFACE,
        offsetField="",
        thicknessAssignment=FROM_SECTION,
    )
    # ------------------------------------------------------------------------------------------------------------------
    # Step---contained steps
    load_time = 12.0
    model.StaticStep(
        name="consolidation",
        previous="Initial",
        description="",
        timePeriod=1e-2,
        timeIncrementationMethod=AUTOMATIC,
        maxNumInc=1000,
        initialInc=1e-2,
        minInc=1e-8,
        maxInc=1e-2,
    )
    model.StaticStep(
        name="Shear",
        previous="consolidation",
        description="",
        timePeriod=load_time,
        timeIncrementationMethod=AUTOMATIC,
        maxNumInc=10000,
        initialInc=1e-6,
        minInc=1e-8,
        maxInc=0.06,
        matrixSolver=DIRECT,
        matrixStorage=UNSYMMETRIC,
        nlgeom=ON,
    )
    # ------------------------------------------------------------------------------------------------------------------
    # Output Request
    instance_output = myassembly.allInstances["instance_soil"].sets["set-all"]
    model.FieldOutputRequest(
        "F-Output-1",
        createStepName="Shear",
        region=instance_output,
        timeInterval=0.06,
        variables=(
            "S",
            "PE",
            "PEEQ",
            "PEMAG",
            "LE",
            "U",
            "RF",
            "CF",
            "CSTRESS",
            "CDISP",
            "SDV",
        ),
    )
    #
    try:
        mdb.models["Model-1"].historyOutputRequests["H-Output-1"].suppress()
    except Exception as error:
        print(error)
    # ------------------------------------------------------------------------------------------------------------------
    # Boundary condition for initial
    bottom_x_instance = model.rootAssembly.instances["instance_soil"].sets[
        "set-X-bottom"
    ]
    model.DisplacementBC(
        name="BC_X_bottom",
        createStepName="Initial",
        region=bottom_x_instance,
        u1=SET,
        u2=UNSET,
        u3=UNSET,
        ur1=UNSET,
        ur2=UNSET,
        ur3=UNSET,
        amplitude=UNSET,
        distributionType=UNIFORM,
        fieldName="",
        localCsys=None,
    )
    #
    bottom_y_instance = model.rootAssembly.instances["instance_soil"].sets[
        "set-Y-bottom"
    ]
    model.DisplacementBC(
        name="BC_Y_bottom",
        createStepName="Initial",
        region=bottom_y_instance,
        u1=UNSET,
        u2=SET,
        u3=UNSET,
        ur1=UNSET,
        ur2=UNSET,
        ur3=UNSET,
        amplitude=UNSET,
        distributionType=UNIFORM,
        fieldName="",
        localCsys=None,
    )
    #
    bottom_z_instance = model.rootAssembly.instances["instance_soil"].sets[
        "set-Z-bottom"
    ]
    model.DisplacementBC(
        name="BC_Z_bottom",
        createStepName="Initial",
        region=bottom_z_instance,
        u1=UNSET,
        u2=UNSET,
        u3=SET,
        ur1=UNSET,
        ur2=UNSET,
        ur3=UNSET,
        amplitude=UNSET,
        distributionType=UNIFORM,
        fieldName="",
        localCsys=None,
    )
    # LOAD--Pressure for consolidation
    surf_p = model.rootAssembly.instances["instance_soil"].surfaces["Surface-P"]
    model.Pressure(
        name="Confining pressure",
        createStepName="consolidation",
        region=surf_p,
        distributionType=UNIFORM,
        field="",
        magnitude=pressure,
        amplitude=UNSET,
    )
    # model.loads['Confining pressure'].setValuesInStep(stepName='Compress', magnitude=pressure)

    # load controlled by strain
    top_x_instance = model.rootAssembly.instances["instance_soil"].sets["set-X-top"]
    model.DisplacementBC(
        name="load-X",
        createStepName="Shear",
        region=top_x_instance,
        u1=-0.12,
        u2=UNSET,
        u3=UNSET,
        ur1=UNSET,
        ur2=UNSET,
        ur3=UNSET,
        amplitude=UNSET,
        distributionType=UNIFORM,
        fieldName="",
        localCsys=None,
    )
    # ------------------------------------------------------------------------------------------------------------------
    # JOB
    my_job = mdb.Job(  # type: ignore[reportUnknownArgumentType]
        name=inp_name,
        model=model_name,
        description="",
        type=ANALYSIS,
        atTime=None,
        waitMinutes=0,
        waitHours=0,
        queue=None,
        memory=90,
        memoryUnits=PERCENTAGE,
        getMemoryFromAnalysis=True,
        explicitPrecision=SINGLE,
        nodalOutputPrecision=SINGLE,
        echoPrint=OFF,
        modelPrint=OFF,
        contactPrint=OFF,
        historyPrint=OFF,
        userSubroutine="",
        scratch="",
        resultsFormat=ODB,
        numThreadsPerMpiProcess=1,
        multiprocessingMode=DEFAULT,
        numCpus=14,
        numDomains=14,
        numGPUs=0,
    )
    # ------------------------------------------------------------------------------------------------------------------
    # Save abaqus model and write input file
    try:
        mdb.saveAs(model_path + "\\" + cae_name + ".cae")
    except Exception as save_error:
        print(save_error)
    # Specify a folder to store input documents in
    os.chdir(test_path)
    # write input file
    my_job.writeInput(consistencyChecking=OFF)

    # ----------------------------------------------------------------------------------------------------------------------
