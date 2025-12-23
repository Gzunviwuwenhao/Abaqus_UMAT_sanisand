# -* - coding:UTF-8 -*-
import os

from typing import List


def input_inp(test_path: str, inp_name: str):
    os.chdir(test_path)
    filename = inp_name + ".inp"
    origin_str_inp = ["*Depvar\n", "** BOUNDARY CONDITIONS\n"]
    insert_str_inp = [
        '1, Voidr, "void ratio: e"\n'
        '2, Hard, "hardening parameter: HARD"\n'
        '3, F11, "FABRIC(1)"\n'
        '4, F22, "FABRIC(2)"\n'
        '5, F33, "FABRIC(3)"\n'
        '6, F12, "FABRIC(4)"\n'
        '7, F13, "FABRIC(5)"\n'
        '8, F23, "FABRIC(6)"\n'
        '9, P, "confining pressure"\n'
        '10, Q, "deviatoric stress"\n'
        '11, R, "Stress Ratio"\n'
        '12, Ev, "volumetric strain"\n'
        '13, Eq, "shear strain"\n'
        '14, ANIV, "ANISOTROPIC VARLABLE :A"\n'
        '15, fmag, "the norm of fabric tensor"\n'
        '16, Kp1, "plastic1 modulus"\n'
        '17, DPLA, " Dilatation1"\n'
        '18, Axial Stress, "axial stress"\n'
        "**\n",
        "** ----------------------------------------------------------------\n"
        "*initial conditions, type=solution, user\n"
        "*initial conditions, type=stress, user\n",
    ]
    insert_rows = [1, 0]
    modify_content(
        filename, origin_str_inp, insert_str_inp, insert_rows, insert_or_replace=1
    )
    #
    return


# ----------------------------------------------------------------------------------------------------------------------
def modify_content(
    filename: str,
    contents_to_search: List[str],
    new_content: List[str],
    insert_row: List[int],
    insert_or_replace: int,
):
    """
    在指定文件的指定位置插入或者替代所需的字符串
    注意:使用的abaqus 2023的版本,abaqus 内置python版本为python2.7
    !!!
    :param filename: 修改的文件名！！！(这里的输入文件名必须带后缀)
    :param contents_to_search: 要搜索的内容列表
    :param new_content: 要插入或替换的新内容列表
    :param insert_row: 插入行偏移列表（仅用于插入模式）
    :param insert_or_replace: ==1 插入字符串
                              ==2 替代字符串
    """
    # ------------------------------------------------------------------------------------------------------------------
    try:
        # open the file
        with open(filename, "r", encoding="utf-8") as file:
            if insert_or_replace == 1:
                # read specify content
                file_lines = file.readlines()
                # 收集所有要插入的位置和内容
                insertions = []
                for i in range(len(contents_to_search)):
                    str_index_inp = file_lines.index(contents_to_search[i])
                    insertions.append((str_index_inp + 1 + insert_row[i], new_content[i]))

                # 从后往前插入，避免索引错位
                insertions.sort(reverse=True, key=lambda x: x[0])
                for index, content in insertions:
                    file_lines.insert(index, content)

                # 一次性写入所有修改
                with open(filename, "w") as new_file:
                    for newline in file_lines:
                        new_file.write(newline)
            elif insert_or_replace == 2:
                file_content = file.read()
                # 遍历替换项并执行替换操作
                # 假设 contents_to_search 和 new_content 是等长的列表
                # 每个 contents_to_search[i] 被替换为 new_content[i]
                for i in range(len(contents_to_search)):
                    file_content = file_content.replace(contents_to_search[i], new_content[i])
                #
                with open(filename, "w") as new_file:
                    new_file.write(file_content)
        #
    except Exception as modify_content_error:
        print("error has occurred:{error}".format(error=modify_content_error))
    #
    return
